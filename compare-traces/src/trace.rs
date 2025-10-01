use std::collections::HashMap;
use std::fmt::Display;

use crate::event::Event;
use crate::tree::{Tree, TreeNodeIndex};

/// Applies 2 space indentation using 1-based depth.
// JRS: The matching `line_depth` function should probably be moved here as well
fn indent_line(f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
    for _ in 0..(depth - 1) {
        write!(f, "  ")?;
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub struct Trace<'content> {
    pub(crate) lines: Vec<&'content str>,
    pub(crate) events: Vec<Event>,
    pub(crate) tree: Tree,
}

impl<'content> Trace<'content> {
    pub fn parse_str(content: &str) -> Trace {
        let lines: Vec<_> = content.lines().collect();
        Self::parse_lines(lines)
    }

    pub fn parse_lines(lines: Vec<&str>) -> Trace {
        let mut parse_errors = vec![];
        let mut events: Vec<_> = lines
            .iter()
            .map(|line| Event::parse(line))
            .filter_map(|r| r.map_err(|e| parse_errors.push(e)).ok())
            .collect();
        for error in parse_errors {
            eprintln!("{}", error);
        }

        // Attach partner events
        if !events.is_empty() {
            for i in 0..(events.len() - 1) {
                let (left, right) = events.split_at_mut(i + 1);
                let a = left.last_mut().unwrap();
                let b = right.first().unwrap();
                a.attach_partner(b);
            }
        }

        // Convert lines into trees
        let tree = Tree::from_indented_items(&lines);

        Trace {
            lines,
            events,
            tree,
        }
    }

    pub fn renumber(&mut self) {
        let old_indices_in_new_order: Vec<TreeNodeIndex> =
            self.tree.dfs_pre_order().map(|node| node.index).collect();

        let new_len = old_indices_in_new_order.len();
        // New length might be smaller, but cannot be larger
        assert!(new_len <= self.events.len());

        let mut old_to_new_indices: HashMap<TreeNodeIndex, TreeNodeIndex> = HashMap::new();

        let mut new_lines: Vec<&'content str> = self.lines.clone();
        new_lines.truncate(new_len);
        let mut new_events: Vec<Event> = self.events.clone();
        new_events.truncate(new_len);

        for i in 0..new_len {
            let old_index = old_indices_in_new_order[i];
            let new_index = TreeNodeIndex::Node(i);

            // Store in map we'll use to rewire the tree in the next pass
            old_to_new_indices.insert(old_index, new_index);

            // Move lines and events into position
            new_lines[new_index.unwrap()] = self.lines[old_index.unwrap()];
            new_events[new_index.unwrap()] = self.events[old_index.unwrap()].clone();
        }

        // Store new lines and events
        self.lines = new_lines;
        self.events = new_events;

        // Remove any dangling nodes
        self.tree
            .nodes
            .retain(|node| old_to_new_indices.contains_key(&node.index));
        assert!(self.tree.nodes.len() == new_len);

        for n in 0..new_len {
            let node = &mut self.tree.nodes[n];

            let old_index = node.index;
            let new_index = old_to_new_indices[&old_index];

            // For each node, update all index-based fields
            node.index = new_index;
            if let Some(old_parent) = node.parent {
                if !old_parent.is_root() {
                    node.parent = Some(old_to_new_indices[&old_parent]);
                }
            }
            for c in 0..node.children.len() {
                let old_child_index = node.children[c];
                node.children[c] = old_to_new_indices[&old_child_index];
            }
        }

        // For the root, update all index-based fields
        let root = &mut self.tree[&Tree::root_index()];
        for i in 0..root.children.len() {
            let old_child_index = root.children[i];
            root.children[i] = old_to_new_indices[&old_child_index];
        }

        // Move nodes into position
        self.tree
            .nodes
            .sort_unstable_by_key(|node| node.index.unwrap());
    }
}

impl Display for Trace<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (node, depth) in self.tree.dfs_pre_order_with_depth() {
            indent_line(f, depth)?;
            let event = node.data(&self.events);
            write!(f, "{}", event)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip() {
        let content = "
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
    RF: xrealloc at wrapper.c:140:1"
            .trim();
        assert_eq!(format!("{}", Trace::parse_str(content)).trim(), content);
    }
}
