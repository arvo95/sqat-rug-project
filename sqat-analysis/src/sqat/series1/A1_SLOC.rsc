module sqat::series1::A1_SLOC

import IO;
import ParseTree;
import String;
import util::FileSystem;
//import sqat::series1::Comments;

/* 

Count Source Lines of Code (SLOC) per file:
- ignore comments
- ignore empty lines

Tips
- use locations with the project scheme: e.g. |project://jpacman/...|
- functions to crawl directories can be found in util::FileSystem
- use the functions in IO to read source files

Answer the following questions:
- what is the biggest file in JPacman?
- what is the total size of JPacman?
- is JPacman large according to SIG maintainability?
- what is the ratio between actual code and test code size?

Sanity checks:
- write tests to ensure you are correctly skipping multi-line comments
- and to ensure that consecutive newlines are counted as one.
- compare you results to external tools sloc and/or cloc.pl

Bonus:
- write a hierarchical tree map visualization using vis::Figure and 
  vis::Render quickly see where the large files are. 
  (https://en.wikipedia.org/wiki/Treemapping) 

*/

alias SLOC = map[loc file, int sloc];

SLOC sloc(loc project) {
  SLOC result = ();
  mainfiles = crawl(project);
  return traverseDirs(mainfiles);
}         

SLOC traverseDirs(FileSystem fs){
	switch(fs){
		case directory(loc l, set[FileSystem] kids): {
			SLOC file_lines = ();
			for (FileSystem kid <- kids) {
				file_lines = file_lines + traverseDirs(kid);
			}
			return file_lines;
		}
		case file(loc l): {
			return (l:size(readFileLines(l)));
		}
	}	
}

int countLines(FileSystem file){
	
}

int countDirs(FileSystem fs)
= (0 | it + 1 | /directory(_, _) := fs );
// := means matching one pattern with another
// / means at any depth of recursion

void main() {
	xs = {<1,2>, <2,3>};
	lolx = (1:2, 2:3);
	
}
             