use std::{
    any,
    cmp::{max, min},
};

fn main() {
    let path_a = vec![
        "ReplicatedStorage".to_string(),
        "SomeFolder".to_string(),
        "SomeScript".to_string(),
        "InstanceInsideScript".to_string(),
    ];
    let path_b = vec![
        "ReplicatedStorage".to_string(),
        "SomeFolder".to_string(),
        "SomeScript".to_string(),
    ];
    let path_c = vec![
        "ReplicatedStorage".to_string(),
        "SomeFolder".to_string(),
        "SomeOtherScript".to_string(),
    ];
    let path_d = vec![
        "ReplicatedStorage".to_string(),
        "SomeOtherFolder".to_string(),
        "SomeOtherScript".to_string(),
    ];
    let path_e = vec![
        "Workspace".to_string(),
        "SomeOtherFolder".to_string(),
        "SomeOtherScript".to_string(),
    ];
    let path_f = vec![
        "Workspace".to_string(),
        "SomeOtherFolder".to_string(),
        "SomeOtherScript".to_string(),
    ];

    println!("a/b {:?}", find_closest_common_ancestor(&path_a, &path_b));
    println!("a/c {:?}", find_closest_common_ancestor(&path_a, &path_c));
    println!("b/d {:?}", find_closest_common_ancestor(&path_b, &path_d));
    println!("a/e {:?}", find_closest_common_ancestor(&path_a, &path_e));
    println!("a/e {:?}", find_closest_common_ancestor(&path_e, &path_f));
}

#[derive(Debug)]
enum PathResult {
    Some(String, usize),
    None,
    Indentical,
}
fn find_closest_common_ancestor(path_a: &Vec<String>, path_b: &Vec<String>) -> PathResult {
    let (larger_path, smaller_path) = if path_a.len() > path_b.len() {
        (path_a, path_b)
    } else {
        (path_b, path_a)
    };

    let mut ancestor: Option<String> = None;
    let mut ancestor_index = 0;
    let mut are_identical = false;
    for (index, larger_name) in larger_path.iter().enumerate() {
        if smaller_path.len() < index + 1 {
            break;
        }
        let smaller_name = &smaller_path[index];

        ancestor_index = index;
        if larger_name.as_str() == smaller_name.as_str() {
            ancestor = Some(larger_name.clone());
            if index == larger_path.len() - 1 {
                are_identical = true;
            }
        } else {
            break;
        }
    }

    match ancestor {
        Some(ancestor_name) => match are_identical {
            true => PathResult::Indentical,
            false => PathResult::Some(ancestor_name, ancestor_index),
        },
        None => PathResult::None,
    }
}
