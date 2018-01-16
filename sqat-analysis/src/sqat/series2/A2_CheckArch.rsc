module sqat::series2::A2_CheckArch

import sqat::series2::Dicto;
import lang::java::jdt::m3::Core;
import Message;
import ParseTree;
import IO;
import String;



/*

This assignment has two parts:
- write a dicto file (see example.dicto for an example)
  containing 3 or more architectural rules for Pacman
  1. BoardPannel.java must import java.awt.Color;
  2. nl.tudelft.jpacman.level.Pellet must inherit nl.tudelft.jpacman.board.Unit
  3. nl.tudelft.jpacman.board.Board must invoke nl.tudelft.jpacman.board.Board.getHeight
  
- write an evaluator for the Dicto language that checks for
  violations of these rules. 

Part 1  

An example is: ensure that the game logic component does not 
depend on the GUI subsystem. Another example could relate to
the proper use of factories.   

Make sure that at least one of them is violated (perhaps by
first introducing the violation).

Explain why your rule encodes "good" design.
  
Part 2:  
 
Complete the body of this function to check a Dicto rule
against the information on the M3 model (which will come
from the pacman project). 

A simple way to get started is to pattern match on variants
of the rules, like so:

switch (rule) {
  case (Rule)`<Entity e1> cannot depend <Entity e2>`: ...
  case (Rule)`<Entity e1> must invoke <Entity e2>`: ...
  ....
}

Implement each specific check for each case in a separate function.
If there's a violation, produce an error in the `msgs` set.  
Later on you can factor out commonality between rules if needed.

The messages you produce will be automatically marked in the Java
file editors of Eclipse (see Plugin.rsc for how it works).

Tip:
- for info on M3 see series2/A1a_StatCov.rsc.

Questions
- how would you test your evaluator of Dicto rules? (sketch a design)
- come up with 3 rule types that are not currently supported by this version
  of Dicto (and explain why you'd need them). 
*/



// from Entity get location
loc getLocationFromEntity(Entity e) { 
	if(contains("<e>", "::")) {
		return |java+method:///| + replaceAll(replaceAll("<e>", ".", "/"), "::", "/");
	}
	return |java+class:///| + replaceAll("<e>", ".", "/");
}


// INVOKES

// check which methods are invoked in given methode 
set[loc] methodInvokeMethods(loc method, M3 m3) {
	set[loc] methodList = {};
	for(<loc from, loc to> <- m3.methodInvocation) {
		if((split("///", split("(", from.uri)[0])[-1]) == (split("///", split("(", method.uri)[0])[-1])) {
			methodList += to;
		}
	}
	return methodList;
}

// check which methods are invoked in given class 
set[loc] classInvokeMethods(loc class, M3 m3) { 
	set[loc] methodsInvoked = {};
	set[loc] methodsInClass = {};
	for(<loc name, loc src> <- m3.declarations) {
		if(split(".", split("/", src.uri)[-1])[0] == split("/", class.uri)[-1]) {
			methodsInClass += name;
		}
	}
	for(loc method <- methodsInClass) {
		methodsInvoked += methodInvokeMethods(method, m3);
	}
	return methodsInvoked;
}

set[loc] invokeHelper (loc loc1, M3 m3) {
	
	set[loc] methods;
	if(isMethod(loc1)) {
		methods = methodInvokeMethods(loc1, m3);
	} else {
		methods = classInvokeMethods(loc1, m3);
	}
	
	return methods;
}

Message mustInvoke(Entity e1, Entity e2, M3 m3) {
	loc loc1 = getLocationFromEntity(e1);
	loc loc2 = getLocationFromEntity(e2);
	if(!isMethod(loc2)) {
		return  warning("not a method", loc2);
	}
	
	set[loc] methods = invokeHelper(loc1, m3);
	
	for(loc l <- methods) {
		if((split("///", loc2.uri)[-1]) == (split("(", split("///", l.uri)[-1])[0])) {
			return warning("Accept must invoke", loc1);
		}
	}
	return warning("Decline <e1> does not invoke <e2>", loc1);
}

Message cannotInvoke(Entity e1, Entity e2, M3 m3) {
	loc loc1 = getLocationFromEntity(e1);
	loc loc2 = getLocationFromEntity(e2);
	if(!isMethod(loc2)) {
		return  warning("not a method", loc2);
	}
	
	
	set[loc] methods = invokeHelper(loc1, m3);
	
	for(loc l <- methods) {
		if((split("///", loc2.uri)[-1]) == (split("///", split("(", l.uri)[0])[-1])) {
			return warning("Decline <e1> invokes <e2>", loc1);
		}
	}
	return warning("Accept cannot invoke", loc1);
}

Message canOnlyInvoke(Entity e1, Entity e2, M3 m3) {
	loc loc1 = getLocationFromEntity(e1);
	loc loc2 = getLocationFromEntity(e2);
	if(!isMethod(loc2)) {
		return  warning("not a method", loc2);
	}
	
	set[loc] methods = invokeHelper(loc1, m3);
	
	for(loc l <- methods) {
		if((split("///", loc2.uri)[-1]) != (split("///", split("(", l.uri)[0])[-1])) {
			return warning("Decline <e1> invokes <e2>", loc1);
		}
	}
	return warning("Accept can only invoke", loc1);
}

// INHERIT

set[loc] inheritance(loc file, m3) {
	set[loc] foundInherits = {};
	for(<loc from, loc to> <- m3.extends) {
		if(from == file) {
			foundInherits += to;
		}
	}
	return foundInherits;
}

Message mustInherit(Entity e1, Entity e2, M3 m3) {
	loc loc1 = getLocationFromEntity(e1);
	loc loc2 = getLocationFromEntity(e2);
	if(isMethod(loc1)) {
		return warning("<e1> not a class", loc1);
	}
	if(isMethod(loc2)) {
		return warning("<e2> not a class", loc2);
	}
	if(loc2 notin inheritance(loc1, m3)) {
		return warning("Decline <e1> does not inherit from <e2>", loc1);
	}
	return warning("Accept  must inherit", loc1);
}




set[Message] eval(start[Dicto] dicto, M3 m3) = eval(dicto.top, m3);

set[Message] eval((Dicto)`<Rule* rules>`, M3 m3) 
  = ( {} | it + eval(r, m3) | r <- rules );
  
set[Message] eval(Rule rule, M3 m3) {
  set[Message] msgs = {};
  
  // to be done
  
  switch (rule) {
  	case (Rule)`<Entity e1> must invoke <Entity e2>`: msgs += mustInvoke(e1, e2, m3);
  	case (Rule)`<Entity e1> cannot invoke <Entity e2>`: msgs += cannotInvoke(e1, e2, m3);
  	case (Rule)`<Entity e1> can only invoke <Entity e2>`: msgs += canOnlyInvoke(e1, e2, m3);
  	case (Rule)`<Entity e1> must inherit <Entity e2>`: msgs += mustInherit(e1, e2, m3);
  	case (Rule)`<Entity e1> cannot inherit <Entity e2>`: msgs += cannotInherit(e1, e2, m3);
  	case (Rule)`<Entity e1> can only inherit <Entity e2>`: msgs += canOnlyInherit(e1, e2, m3);
  	case (Rule)`<Entity e1> must import <Entity e2>`: msgs += mustImport(e1, e2, m3);
  	case (Rule)`<Entity e1> cannot import <Entity e2>`: msgs += cannotImport(e1, e2, m3);
  	case (Rule)`<Entity e1> can only import <Entity e2>`: msgs += canOnlyImport(e1, e2, m3);
  	
  }
  
  return msgs;
}

M3 jpacmanM3() = createM3FromEclipseProject(|project://jpacman-framework/src|);

set[Message] runDicto() {
	return eval(parse(#start[Dicto], |project://sqat-analysis/src/sqat/series2/example.dicto|), jpacmanM3());
}


