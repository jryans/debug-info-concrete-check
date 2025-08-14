use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Ok, Result};
// use log::log_enabled;
use serde::Deserialize;
use serde_yaml::Value;

use crate::event::Location;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub enum RemarkType {
    CallRemoved,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
// TODO: Store both call from and call to...?
pub struct Remark {
    pub remark_type: RemarkType,
    pub pass: String,
    pub location: Location,
}

pub fn load_remarks(remarks_file: &PathBuf) -> Result<HashMap<Location, Remark>> {
    println!("Loading remarks…");
    println!();

    let mut remarks_by_location: HashMap<Location, Remark> = HashMap::new();

    let remarks_content = fs::read_to_string(remarks_file).with_context(|| {
        format!(
            "Unable to read optimisation remarks ({})",
            remarks_file.display()
        )
    })?;

    let mut docs: usize = 0;
    for remark_doc in serde_yaml::Deserializer::from_str(&remarks_content) {
        docs += 1;

        if docs % 1000 == 0 {
            println!("{} remarks, continuing…", docs);
        }

        let remark = Value::deserialize(remark_doc)?;
        // if log_enabled!(log::Level::Debug) {
        //     println!("{:#?}", remark);
        //     println!();
        // }

        // Example:
        //
        // TaggedValue {
        //     tag: !Passed,
        //     value: Mapping {
        //         "Pass": String("EarlyCSEPass"),
        //         "Name": String("CallRemoved"),
        //         "DebugLoc": Mapping {
        //             "File": String("path.c"),
        //             "Line": Number(826),
        //             "Column": Number(11),
        //         },
        //         "Function": String("enter_repo"),
        //         "Args": Sequence [
        //             Mapping {
        //                 "String": String("call from "),
        //             },
        //             Mapping {
        //                 "Caller": String("enter_repo"),
        //                 "DebugLoc": Mapping {
        //                     "File": String("path.c"),
        //                     "Line": Number(792),
        //                     "Column": Number(0),
        //                 },
        //             },
        //             Mapping {
        //                 "String": String(" to "),
        //             },
        //             Mapping {
        //                 "Callee": String("strlen"),
        //             },
        //             Mapping {
        //                 "String": String(" removed by "),
        //             },
        //             Mapping {
        //                 "String": String("EarlyCSEPass"),
        //             },
        //         ],
        //     },
        // }

        // Find the remark body within the tag
        let body = match remark {
            Value::Tagged(ref tagged_value) => tagged_value.value.as_mapping().unwrap(),
            _ => continue,
        };

        // For now, we focus on only certain remarks of interest
        if body.get("Name").unwrap().as_str().unwrap() != "CallRemoved" {
            continue;
        }

        let pass = body.get("Pass").unwrap().as_str().unwrap().to_owned();

        let function = body.get("Function").unwrap().as_str().unwrap().to_owned();
        let location = body.get("DebugLoc").unwrap().as_mapping().unwrap();
        // TODO: Need to consider directories as well
        // Strip off parent path components for now
        let file_path = location.get("File").unwrap().as_str().unwrap().to_owned();
        let file = Path::new(&file_path)
            .file_name()
            .unwrap()
            .to_owned()
            .into_string()
            .unwrap();
        let line = location.get("Line").unwrap().as_u64().unwrap();
        let column = location.get("Column").unwrap().as_u64().unwrap();

        let parsed_remark = Remark {
            remark_type: RemarkType::CallRemoved,
            pass,
            location: Location {
                function: Some(function),
                file: Some(file),
                line: Some(line),
                column: Some(column),
            },
        };

        // if log_enabled!(log::Level::Debug) {
        //     println!("{:#?}", remark);
        //     println!();
        // }
        // if log_enabled!(log::Level::Debug) {
        //     println!("{:#?}", parsed_remark);
        //     println!();
        // }

        // TODO: Handle multiple remarks at the same location
        remarks_by_location.insert(parsed_remark.location.clone(), parsed_remark);
    }

    println!("{} remarks, complete!", docs);
    println!();

    Ok(remarks_by_location)
}
