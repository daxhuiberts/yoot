for file in examples/*; do cargo run -- $file > regression/$(basename -s .yoot $file).txt; done
