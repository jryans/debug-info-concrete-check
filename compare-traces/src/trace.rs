use crate::event::Event;
use crate::tree::Tree;

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
