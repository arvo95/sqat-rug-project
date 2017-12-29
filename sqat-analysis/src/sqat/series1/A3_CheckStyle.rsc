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

Chosen checks: AvoidStaticImport, AvoidEscapedUnicodeCharacters, ArrayTrailingComma

Plus: invent your own style violation or code smell and write a checker.

Our style violation - LongVariableNames

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
  text(checkStyle(project));
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
  map[loc, str] unicodeExtracts = ();
  map[loc, ArrayInit] trailingCommaExtracts = ();
  map[loc, ImportDec] staticImportExtracts = ();
  map[loc, int] longVariableExtracts = ();
  set[Message] results = {};
  
  for(loc location <- getFiles(crawl(project)), location.extension == "java") {
  	 unicodeExtracts += extractAvoidEscapedUnicodeCharacters(parse(#start[CompilationUnit], location, allowAmbiguity=true));
  	 trailingCommaExtracts += extractArrayTrailingComma(parse(#start[CompilationUnit], location, allowAmbiguity=true));
  	 staticImportExtracts += extractAvoidStaticImport(parse(#start[CompilationUnit], location, allowAmbiguity=true));
  }
  
  results = synthesizeAvoidEscapedUnicodeCharacters(analyzeAvoidEscapedUnicodeCharacters(unicodeExtracts));
  results += synthesizeArrayTrailingComma(trailingCommaExtracts);
  results += synthesizeAvoidStaticImport(staticImportExtracts);
  return results;
}

map[loc, ImportDec] extractAvoidStaticImport(start[CompilationUnit] cu) {
	map[loc, ImportDec] result = ();
	visit(cu) {
		case staticImport:(ImportDec)`import static <TypeName typename>.<Id identity>;`: {
			result += (staticImport@\loc : staticImport);
			}
		case staticImport:(ImportDec)`import static <TypeName typename>.*;`: {
			result += (staticImport@\loc : staticImport);
			}
	}
	return result;
}


set[Message] synthesizeAvoidStaticImport(map[loc, ImportDec] imports)
	= { warning("Static import detected!", l) | l <- imports };


map[loc, ArrayInit] extractArrayTrailingComma(start[CompilationUnit] cu) {
	map[loc, ArrayInit] result = ();
	visit(cu) {
		case array:(ArrayInit)`{<{VarInit  ","}* var>}`: {
			result += (array@\loc : array);
			}
	}
	return result;
}

set[Message] synthesizeArrayTrailingComma(map[loc, ArrayInit] arrays)
	= { warning("Array without trailing comma detected!", l) | l <- arrays };

map[loc, str] extractAvoidEscapedUnicodeCharacters(start[CompilationUnit] cu) {
	map[loc, str] result = ();
	visit(cu) {
		case literal:(StringLiteral)`<LEX_StringLiteral lex>`: {
			result += (literal@\loc : unparse(lex));
			}
	}
	return result;
}

map[loc, str] analyzeAvoidEscapedUnicodeCharacters(map[loc, str] strings)
	= (l: strings[l] | loc l <- strings, /\/u[a-z0-9]{4}/ := strings[l]);


set[Message] synthesizeAvoidEscapedUnicodeCharacters(map[loc, str] strings)
	= { warning("Escaped unicode character detected!", l) | l <- strings };


map[loc, int] extractLongVariableName() {

}

map[loc, int] analyzeLongVariableName() {

}

set[Message] synthesizeLongVariableName(list[str] lines) {

}

test bool UnicodeCharacter()
	= analyzeAvoidEscapedUnicodeCharacters((|project://1|:"No unicode here", |project://2|:"   /uaf48coolbeans  ")) == (|project://2|:"   /uaf48coolbeans  ");
