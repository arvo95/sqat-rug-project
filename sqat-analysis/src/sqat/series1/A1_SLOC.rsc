module sqat::series1::A1_SLOC

import IO;
import ParseTree;
import String;
import util::FileSystem;
import Map;
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
The biggest java file is "Level.java"
- what is the total size of JPacman?
2466 lines of code
- is JPacman large according to SIG maintainability?
No, it is very small.
- what is the ratio between actual code and test code size?
565:1901 or 29.72% of code is test code.

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
			if (/.*\.java/ := l.uri) {
				return (l:countLines(readFileLines(l)));
			}
			else {
				return ();
			}	
		}
	}	
}

int countLines(list[str] lines){
	int lineCounter = 0;
	bool multiline_comment = false;
	for (str line <- lines) {
		if (!multiline_comment){
			if (/^[\s\t]*\/\*/ := line) { //Detect a multiline comment starting
				multiline_comment = true;
			}
			else if (/^[\s\t]*[^\s\t].*\/\*/ := line) { //Detect multiline comment start with additional content
				multiline_comment = true;
				lineCounter += 1;	
			}
			else if (/^[\s\t]*([^\/\s\t]|[\/][^\/])/ := line) { //Detect non-empty and not single line comment lines
				lineCounter += 1;
			}
		}
		else if (multiline_comment){
			if(/\*\// := line) { //Detect a multiline closing line
				if (!/\*\/.*\*\// := line){
					multiline_comment = false;
				}
				if(/\*\/[\s\t]*([^\s\t\/]|\/[^*])/ := line) { //Detect a multiline closing and new opening in the same line
					lineCounter += 1;
				}
			}
			//TODO: A case with one multiline comment ending, content in between and another multiline starting
		}
		
	} 
	return lineCounter;
}

test bool skippingManyempty()
	= countLines(["aa", "", "   ", "   ", "bb"]) == 2;
	
test bool multilineDetect()
	= countLines(["aa", "/*", "Comment", "*/", "bb"]) == 2;
	
test bool singleLineComment()
	= countLines(["//epic comment", "aa", "bb"]) == 2;

test bool multilineStartsAndEnds()
	= countLines(["/*sample", "*com", "ment*/ /*comment", "*/", "aa"]) == 1; //Counts an extra line somehow
	
test bool multilineStartsAndEndsWithcontent()
	= countLines(["/*sample", "*com", "ment*/ content /*comment", "*/", "aa"]) == 2; //Counts an extra line somehow
	
             