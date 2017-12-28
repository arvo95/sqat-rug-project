module sqat::series1::A3_CheckStyle

import Java17ish;
import Message;
import util::ResourceMarkers;
import IO;
import util::FileSystem;
import ParseTree;
import util::ValueUI;

/*

Assignment: detect style violations in Java source code.
Select 3 checks out of this list:  http://checkstyle.sourceforge.net/checks.html
Compute a set[Message] (see module Message) containing 
check-style-warnings + location of  the offending source fragment. 

Chosen checks: LineLength, AvoidEscapedUnicodeCharacters, ArrayTrailingComma

Plus: invent your own style violation or code smell and write a checker.

Note: since concrete matching in Rascal is "modulo Layout", you cannot
do checks of layout or comments (or, at least, this will be very hard).

JPacman has a list of enabled checks in checkstyle.xml.
If you're checking for those, introduce them first to see your implementation
finds them.

Questions
- for each violation: look at the code and describe what is going on? 
  Is it a "valid" violation, or a false positive?

Tips 

- use the grammar in lang::java::\syntax::Java15 to parse source files
  (using parse(#start[CompilationUnit], aLoc), in ParseTree)
  now you can use concrete syntax matching (as in Series 0)

- alternatively: some checks can be based on the M3 ASTs.

- use the functionality defined in util::ResourceMarkers to decorate Java 
  source editors with line decorations to indicate the smell/style violation
  (e.g., addMessageMarkers(set[Message]))

  
Bonus:
- write simple "refactorings" to fix one or more classes of violations 

*/

void main(loc project) {
  addMessageMarkers(checkStyle(project));
}

set[loc] getFiles(FileSystem fs) {
	set[loc] files = {};
	switch(fs){
		case directory(loc l, set[FileSystem] kids): {
			for (FileSystem kid <- kids) {
				files = files + getFiles(kid);
			}
			return files;
		}
		case file(loc l): {
			return {l};
		}
	}
	return files;
}

set[Message] checkStyle(loc project) {
  map[loc, str] result = ();
  for(loc location <- getFiles(crawl(project)), location.extension == "java") {
  	 result += extractAvoidEscapedUnicodeCharacters(parse(#start[CompilationUnit], location, allowAmbiguity=true));
  }
  
  return synthesizeAvoidEscapedUnicodeCharacters(analyzeAvoidEscapedUnicodeCharacters(result));
}

/*extractLineLength() {

}*/

/*analyzeLineLength() {

}*/

/*set[Message] synthesizeLineLength(list[str] lines) {

}*/

/*extractArrayTrailingComma() {

}*/

/*analyzeArrayTrailingComma() {

}*/

/*set[Message] synthesizeArrayTrailingComma(list[str] lines) {

}*/

map[loc, str] extractAvoidEscapedUnicodeCharacters(start[CompilationUnit] cu) {
	map[loc, str] result = ();
	visit(cu) {
		case literal:(StringLiteral)`<LEX_StringLiteral lex>`: {
			result += (literal@\loc : unparse(lex));
			}
	}
	return result;
}

/*map[loc, str] extractAvoidEscapedUnicodeCharacters() {

}*/

map[loc, str] analyzeAvoidEscapedUnicodeCharacters(map[loc, str] strings)
	= (l: strings[l] | loc l <- strings, /\/u[a-z0-9]{4}/ := strings[l]);


set[Message] synthesizeAvoidEscapedUnicodeCharacters(map[loc, str] strings)
	= { warning("Escaped unicode character detected!", l) | l <- strings };


/*extractCustomCheck() {

}*/

/*analyzeCustomCheck() {

}*/

/*set[Message] synthesizeCustomCheck(list[str] lines) {

}*/

test bool UnicodeCharacter()
	= analyzeAvoidEscapedUnicodeCharacters((|project://1|:"No unicode here", |project://2|:"   /uaf48coolbeans  ")) == (|project://2|:"   /uaf48coolbeans  ");