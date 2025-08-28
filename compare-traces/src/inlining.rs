use std::collections::{HashMap, HashSet};

use crate::event::{Event, EventSource, EventType, Location};
use crate::trace::Trace;
use crate::tree::TreeNodeIndex;

#[derive(PartialEq, Eq, Hash, Debug)]
struct CallEdge {
    from_location: Location,
    to_function: String,
}

fn event_to_call_edge(event: &Event) -> Option<CallEdge> {
    if event.event_type != EventType::CallFrom {
        return None;
    }
    let call_from_event = event;
    let from_location = &call_from_event.location;
    // Call from event must have full set of coordinates
    if from_location.function.is_none()
        || from_location.file.is_none()
        || from_location.line.is_none()
        || from_location.column.is_none()
    {
        return None;
    }
    // Call from line must be non-zero
    if from_location.line.unwrap() == 0 {
        return None;
    }
    if call_from_event.partner.is_none() {
        println!("⚠️ Call from event without partner: {}", call_from_event);
        return None;
    }
    let partner_event = call_from_event.partner.as_ref().unwrap();
    assert!(partner_event.event_type == EventType::CallTo);
    let call_to_event = partner_event;
    let to_location = &call_to_event.location;
    // Call to event only needs to have function name
    if to_location.function.is_none() {
        return None;
    }
    Some(CallEdge {
        from_location: call_from_event.location.clone(),
        to_function: call_to_event.location.function.as_ref().unwrap().clone(),
    })
}

fn gather_inlined_call_edges(trace: &Trace<'_>) -> HashSet<CallEdge> {
    let mut inlined_call_edges: HashSet<CallEdge> = HashSet::new();
    for event in &trace.events {
        if event.event_source != EventSource::InlinedChain {
            continue;
        }
        if let Some(edge) = event_to_call_edge(event) {
            let partner_event = event.partner.as_ref().unwrap();
            assert!(partner_event.event_source == EventSource::InlinedChain);
            inlined_call_edges.insert(edge);
        }
    }
    inlined_call_edges
}

fn cluster_inlined_call_children(trace: &mut Trace<'_>, inlined_call_edges: &HashSet<CallEdge>) {
    let mut inlined_call_edge_to_first_instance: HashMap<CallEdge, TreeNodeIndex> = HashMap::new();
    let mut current_parent: Option<TreeNodeIndex> = None;
    // Proceed through the tree breadth-first using mutable iterator
    for (tree, node_index) in trace.tree.bfs_mut() {
        // println!("node: {:?}", node_index);
        let event = {
            let node = &tree[&node_index];
            // If we've changed parents, clear past state of call edges
            if node.parent != current_parent {
                // println!("Parent changed, reset call edge instance map");
                inlined_call_edge_to_first_instance.clear();
                current_parent = node.parent;
            }
            node.data(&trace.events)
        };
        // Within each parent, check whether any direct children are inlined call edges
        if let Some(edge) = event_to_call_edge(event) {
            // The current trace's call edges might not be inlined themselves,
            // so we also check with previously gathered set
            if !inlined_call_edges.contains(&edge) {
                continue;
            }
            // If this edge is new for this parent,
            // track it as we may move children of other instances here
            if !inlined_call_edge_to_first_instance.contains_key(&edge) {
                inlined_call_edge_to_first_instance.insert(edge, node_index);
                continue;
            }
            // When an inlined call edge is found multiple times within the same parent, move all
            // children of subsequent matching call edges to the first instance of that edge
            let mut children = {
                let node = &mut tree[&node_index];
                let cloned_children = node.children.clone();
                node.children.clear();
                cloned_children
            };
            let new_children_parent_index: TreeNodeIndex =
                inlined_call_edge_to_first_instance[&edge];
            // println!(
            //     "Moving children {:?} from {:?} to {:?}",
            //     children, node_index, new_children_parent_index
            // );
            // Update child to parent link in each child
            for child in &children {
                tree[child].parent = Some(new_children_parent_index);
            }
            let new_children_parent = &mut tree[&new_children_parent_index];
            new_children_parent.children.append(&mut children);
            // The current node is no longer needed, so remove from its parent
            let parent_index = tree[&node_index].parent.unwrap();
            let parent = &mut tree[&parent_index];
            let current_position = parent
                .children
                .iter()
                .position(|child_index| *child_index == node_index)
                .unwrap();
            parent.children.remove(current_position);
            // println!(
            //     "Removed current node {:?} from its parent {:?}",
            //     node_index, parent_index
            // );
        }
    }
    // println!();
}

pub fn preprocess_inlining(before: &mut Trace<'_>, after: &mut Trace<'_>) {
    // Gather all inlined call edges in after trace
    let inlined_call_edges = gather_inlined_call_edges(after);
    // Cluster inlined call children in each trace
    cluster_inlined_call_children(before, &inlined_call_edges);
    cluster_inlined_call_children(after, &inlined_call_edges);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inlined_call_edges_found() {
        let content = "
ICF: do_git_config_sequence at config.c:2126:24
  ICT: git_system_config at config.c:2072:0
  CF: git_system_config at config.c:2076:19
    CT: system_path at exec-cmd.c:262:0
    CF: system_path at exec-cmd.c:268:2
      CT: strbuf_addf at strbuf.c:333:0
      CF: strbuf_addf at strbuf.c:336:2
        CT: strbuf_vaddf at strbuf.c:390:0
        ICF: strbuf_vaddf at strbuf.c:394:7
          ICT: strbuf_avail at strbuf.h:139:0
          IRF: strbuf_avail at strbuf.h:0:0
        ICF: strbuf_vaddf at strbuf.c:395:3
          ICT: strbuf_grow at strbuf.c:91:0
          IRF: strbuf_grow at strbuf.c:0:0
        ICF: strbuf_vaddf at strbuf.c:394:7
          ICT: strbuf_avail at strbuf.h:139:0
          IRF: strbuf_avail at strbuf.h:0:0"
            .trim();
        let trace = Trace::parse_str(content);
        let inlined_call_edges = gather_inlined_call_edges(&trace);
        assert_eq!(inlined_call_edges.len(), 3);
    }

    #[test]
    fn inlined_call_children_clustered() {
        let content = "
CT: strbuf_vaddf at strbuf.c:390:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  CF: strbuf_grow at strbuf.c:99:2
    CT: xrealloc at wrapper.c:127:0
    ICF: xrealloc at wrapper.c:135:2
      ICT: memory_limit_check at wrapper.c:17:0
      IRF: memory_limit_check at wrapper.c:0:0
    CF: xrealloc at wrapper.c:136:8
      CT: Jump to external code for realloc
      RF: Jump to external code for realloc
    RF: xrealloc at wrapper.c:140:1
  IRF: strbuf_grow at strbuf.c:0:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0
CF: strbuf_vaddf at strbuf.c:397:8
  CT: Jump to external code for vsnprintf
  RF: Jump to external code for vsnprintf
ICF: strbuf_vaddf at strbuf.c:401:12
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:407:2
  ICT: strbuf_setlen at strbuf.h:160:0
  IRF: strbuf_setlen at strbuf.h:0:0
RF: strbuf_vaddf at strbuf.c:408:1"
            .trim();
        let mut trace = Trace::parse_str(content);
        let inlined_call_edges = gather_inlined_call_edges(&trace);
        cluster_inlined_call_children(&mut trace, &inlined_call_edges);
        let expected = "
CT: strbuf_vaddf at strbuf.c:390:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0
  ICT: strbuf_grow at strbuf.c:91:0
  CF: strbuf_grow at strbuf.c:99:2
    CT: xrealloc at wrapper.c:127:0
    ICF: xrealloc at wrapper.c:135:2
      ICT: memory_limit_check at wrapper.c:17:0
      IRF: memory_limit_check at wrapper.c:0:0
    CF: xrealloc at wrapper.c:136:8
      CT: Jump to external code for realloc
      RF: Jump to external code for realloc
    RF: xrealloc at wrapper.c:140:1
  IRF: strbuf_grow at strbuf.c:0:0
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0
CF: strbuf_vaddf at strbuf.c:397:8
  CT: Jump to external code for vsnprintf
  RF: Jump to external code for vsnprintf
ICF: strbuf_vaddf at strbuf.c:401:12
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:407:2
  ICT: strbuf_setlen at strbuf.h:160:0
  IRF: strbuf_setlen at strbuf.h:0:0
RF: strbuf_vaddf at strbuf.c:408:1"
            .trim();
        assert_eq!(format!("{}", trace).trim(), expected);
    }

    #[test]
    fn inlined_call_children_clustered_iteration() {
        // Ensure we follow proper breadth-first order
        // even after we've starting modifying the tree
        let content = "
CT: strbuf_vaddf at strbuf.c:390:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  ICF: strbuf_grow at strbuf.c:99:2
    ICT: xrealloc at wrapper.c:127:0
    IRF: xrealloc at wrapper.c:140:1
  IRF: strbuf_grow at strbuf.c:0:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  ICF: strbuf_grow at strbuf.c:99:2
    ICT: xrealloc at wrapper.c:127:0
    IRF: xrealloc at wrapper.c:140:1
  IRF: strbuf_grow at strbuf.c:0:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0"
            .trim();
        let mut trace = Trace::parse_str(content);
        let inlined_call_edges = gather_inlined_call_edges(&trace);
        cluster_inlined_call_children(&mut trace, &inlined_call_edges);
        let expected = "
CT: strbuf_vaddf at strbuf.c:390:0
ICF: strbuf_vaddf at strbuf.c:394:7
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
  ICT: strbuf_avail at strbuf.h:139:0
  IRF: strbuf_avail at strbuf.h:0:0
ICF: strbuf_vaddf at strbuf.c:395:3
  ICT: strbuf_grow at strbuf.c:91:0
  ICF: strbuf_grow at strbuf.c:99:2
    ICT: xrealloc at wrapper.c:127:0
    IRF: xrealloc at wrapper.c:140:1
    ICT: xrealloc at wrapper.c:127:0
    IRF: xrealloc at wrapper.c:140:1
  IRF: strbuf_grow at strbuf.c:0:0
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0
  ICT: strbuf_grow at strbuf.c:91:0
  IRF: strbuf_grow at strbuf.c:0:0"
            .trim();

        // println!("Result:\n\n{}", format!("{}", trace).trim());
        // println!();
        // println!("Expected:\n\n{}", expected);
        // println!();

        assert_eq!(format!("{}", trace).trim(), expected);

        // Should be the same after renumbering as well
        trace.renumber();
        assert_eq!(format!("{}", trace).trim(), expected);
    }
}
