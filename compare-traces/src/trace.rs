use std::fmt::Display;

use crate::event::Event;
use crate::tree::Tree;

/// Applies 2 space indentation using 1-based depth.
// JRS: The matching `line_depth` function should probably be moved here as well
fn indent_line(f: &mut std::fmt::Formatter<'_>, depth: usize) -> std::fmt::Result {
    for _ in 0..(depth - 1) {
        write!(f, "  ")?;
    }
    Ok(())
}

pub struct Trace<'content> {
    pub(crate) lines: Vec<&'content str>,
    pub(crate) events: Vec<Event>,
    pub(crate) tree: Tree,
}

impl Trace<'_> {
    pub fn parse(content: &str) -> Trace {
        let lines: Vec<_> = content.lines().collect();
        let mut events: Vec<_> = lines
            .iter()
            .map(|line| Event::parse(line).unwrap())
            .collect();

        // Attach partner events
        for i in 0..(events.len() - 1) {
            let (left, right) = events.split_at_mut(i + 1);
            let a = left.last_mut().unwrap();
            let b = right.first().unwrap();
            a.attach_partner(b);
        }

        // Convert lines into trees
        let tree = Tree::from_indented_items(&lines);

        Trace {
            lines,
            events,
            tree,
        }
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
        assert_eq!(format!("{}", Trace::parse(content)).trim(), content);
    }
}
