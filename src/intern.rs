use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::marker::PhantomData;

static NEXT_INTERN_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Id {
    id: usize,
    intern: usize,
}

#[derive(Debug)]
pub struct Intern {
    str_to_id: HashMap<String, Id>,
    id_to_str: Vec<String>,
    my_id: usize,
    next_id: usize,
}

impl Intern {
    pub fn new() -> Self {
        Self::new_with_capacity(256)
    }

    pub fn new_with_capacity(capacity: usize) -> Self {
        Intern {
            str_to_id: HashMap::with_capacity(capacity),
            id_to_str: Vec::with_capacity(capacity),
            my_id: NEXT_INTERN_ID.fetch_add(1, Ordering::Relaxed),
            next_id: 0,
        }
    }

    pub fn intern(&mut self, val: &str) -> Id {
        if let Some(id) = self.str_to_id.get(val) {
            *id
        } else {
            self.next_id += 1;
            let id = Id {
                id: self.next_id - 1,
                intern: self.my_id,
            };
            let owned = val.to_string();
            self.str_to_id.insert(owned.clone(), id);
            self.id_to_str.push(owned);
            id
        }
    }

    pub fn lookup(&self, id: Id) -> Option<&str> {
        if self.my_id != id.intern {
            panic!("Tried to look up an ID in the wrong intern!");
        };
        self.id_to_str.get(id.id).map(|s| s.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_intern() {
        let mut intern = Intern::new();
        let str_1 = "Hello ";
        let str_2 = "world!";
        let id_1 = intern.intern(str_1);
        let id_2 = intern.intern(str_2);
        assert_eq!(str_1, intern.lookup(id_1).unwrap());
        assert_eq!(str_2, intern.lookup(id_2).unwrap());
        let id_3 = intern.intern(str_1);
        assert_eq!(id_1, id_2);
    }
}