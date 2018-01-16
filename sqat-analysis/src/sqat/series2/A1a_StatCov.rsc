module sqat::series2::A1a_StatCov

import lang::java::jdt::m3::Core;
import analysis::m3::Core;
import util::ValueUI;
import Relation;
import IO;
import List;
import Set;
import String;
import util::Math;
import Type;


/*

Implement static code coverage metrics by Alves & Visser 
(https://www.sig.eu/en/about-sig/publications/static-estimation-test-coverage)


The relevant base data types provided by M3 can be found here:

- module analysis::m3::Core:

rel[loc name, loc src]        M3.declarations;            // maps declarations to where they are declared. contains any kind of data or type or code declaration (classes, fields, methods, variables, etc. etc.)
rel[loc name, TypeSymbol typ] M3.types;                   // assigns types to declared source code artifacts
rel[loc src, loc name]        M3.uses;                    // maps source locations of usages to the respective declarations
rel[loc from, loc to]         M3.containment;             // what is logically contained in what else (not necessarily physically, but usually also)
list[Message]                 M3.messages;                // error messages and warnings produced while constructing a single m3 model
rel[str simpleName, loc qualifiedName]  M3.names;         // convenience mapping from logical names to end-user readable (GUI) names, and vice versa
rel[loc definition, loc comments]       M3.documentation; // comments and javadoc attached to declared things
rel[loc definition, Modifier modifier] M3.modifiers;     // modifiers associated with declared things

- module  lang::java::m3::Core:

rel[loc from, loc to] M3.extends;            // classes extending classes and interfaces extending interfaces
rel[loc from, loc to] M3.implements;         // classes implementing interfaces
rel[loc from, loc to] M3.methodInvocation;   // methods calling each other (including constructors)
rel[loc from, loc to] M3.fieldAccess;        // code using data (like fields)
rel[loc from, loc to] M3.typeDependency;     // using a type literal in some code (types of variables, annotations)
rel[loc from, loc to] M3.methodOverrides;    // which method override which other methods
rel[loc declaration, loc annotation] M3.annotations;

Tips
- encode (labeled) graphs as ternary relations: rel[Node,Label,Node]
- define a data type for node types and edge types (labels) 
- use the solve statement to implement your own (custom) transitive closure for reachability.

Questions:
- what methods are not covered at all?
- how do your results compare to the jpacman results in the paper? Has jpacman improved?
- use a third-party coverage tool (e.g. Clover) to compare your results to (explain differences)
Clover says that the test coverage of jpacman methods is 82.6%

*/


data Node
	= method(loc location)
	| class(loc location)
	| package(loc location)
	| interface(loc location);
	
data Label
	= dMethod()
	| call()
	| dType()
	| vCall();

alias graph = rel[Node, Node];
alias method = tuple[loc name, loc src];
alias class = tuple[loc name, loc src];

set[method] getTestMethods(M3 pacmanM3) {
	return toSet([m | method m <- pacmanM3.declarations, contains(m.src.path, "/test/"), isMethod(m.name)]);
}

set[method] getAllMethods(M3 pacmanM3) {
	return toSet([m | method m <- pacmanM3.declarations, isMethod(m.name)]);
}

set[method] getAllProductionMethods(M3 pacmanM3) {
	return getAllMethods(pacmanM3) - getTestMethods(pacmanM3);
}

graph transitiveClosure(graph g) {
	return solve(g){
		g = g + (g o g);
	};
}

graph createCallGraph(M3 pacmanM3) {
	set[loc] packages = packages(pacmanM3);
    set[loc] classes = classes(pacmanM3);
    set[loc] interfaces = interfaces(pacmanM3);
    set[loc] methods = methods(pacmanM3);

	rel[loc, loc] contains = pacmanM3.containment;
	rel[loc, loc] calls = pacmanM3.methodInvocation;
	rel[loc, loc] overriding = pacmanM3.methodOverrides;
	
	graph g = {};
	
	for(<container, content> <- contains){
		if(container in packages) {
			if(content in classes) {
				g += <class(container), method(content)>;
				//TODO: Add virtual calls
			}
			else {
				g += <interface(container), method(content)>;
			}
		}
	}
	
	for(<invoker, calledFunction> <- calls) {
		if(isConstructor(invoker)) {
			//loc containingClass = 
			if(invoker in interfaces) {
				g += <interface(invoker), method(calledFunction)>;
			}
			else {
				g += <class(invoker), method(calledFunction)>;
			}
		}
		else {
			g += <method(invoker), method(calledFunction)>;
		}
	}
	return g;
}

tuple[num covered, num total] calculateClassCoverage(graph g, M3 pacmanM3, loc coverClass) {
	rel[loc, loc] contains = pacmanM3.containment;
	set[loc] classMethods = toSet([content | <container, content> <- contains, container == coverClass && isMethod(content)]);
	set[method] testMethods = getTestMethods(pacmanM3);
	set[loc] reachedMethods = {};
	
	reachedMethods = toSet([tested | testMethod <- testMethods, tested <- classMethods, <method(testMethod.name), method(tested)> in g]);
	return <size(reachedMethods), size(classMethods)>;
}

tuple[num covered, num total] calculatePackageCoverage(graph g, M3 pacmanM3, loc coverPackage) {
	//tuple[num covered, num total] currentClass = 
	for(<from, to> <- g) {
		if(equivalent(from, class())) { //TODO: Exclude test classes
			calculateClassCoverage(g, pacmanM3, from);
		}
	}
}

num calculateSystemCoverage(graph g, M3 pacmanM3) {
	for(<from, to> <- g) {
		if(equivalent(from, package())) { //TODO:Exclude test packages
			calculatePackageCoverage(g, pacmanM3, from);
		}
	}
}

void printSystemCoverage(graph g, M3 pacmanM3) {
	print(calculateSystemCoverage(g, pacmanM3));
}

void printPackageCoverage(graph g, M3 pacmanM3) {
	
}

void printClassCoverage(graph g, M3 pacmanM3) {
	rel[loc, num] coverage = {};
	set[loc] classes = classes(pacmanM3);
	num percentageCovered = 0;
	productionClasses = {c | c <- classes, !contains(c.path, "/test/")};
	for(productionClass <- productionClasses) {
		tuple[num covered, num total] currentClass = calculateClassCoverage(g, pacmanM3, productionClass);
		if(currentClass.total > 0) {
			percentageCovered = 100 * currentClass.covered / currentClass.total;
		}
		else {
			percentageCovered = 0;
		}
		coverage += <productionClass, percentageCovered>;
	}
	text(coverage);
}

void main(){
	M3 jpacmanM3() = createM3FromEclipseProject(|project://jpacman-framework|);
	graph g = transitiveClosure(createCallGraph(jpacmanM3()));
	//printSystemCoverage(g);
	printClassCoverage(g, jpacmanM3());
	//printPackageCoverage(g);
}
