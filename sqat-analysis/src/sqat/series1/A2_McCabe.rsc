module sqat::series1::A2_McCabe

import lang::java::jdt::m3::AST;
import analysis::statistics::Correlation;
import IO;


/*

Construct a distribution of method cylcomatic complexity. 
(that is: a map[int, int] where the key is the McCabe complexity, and the value the frequency it occurs)


Questions:
- which method has the highest complexity (use the @src annotation to get a method's location)

  <|project://jpacman-framework/src/main/java/nl/tudelft/jpacman/npc/ghost/Inky.java|(2255,2267,<68,1>,<131,17>),8>,

- how does pacman fare w.r.t. the SIG maintainability McCabe thresholds?

	All methods are bellow 10 cyclomatic complexity so it means they are in 1-10 CC category, 
	labeled 'simple, without severe risk
	

- is code size correlated with McCabe in this case (use functions in analysis::statistics::Correlation to find out)? 
  (Background: Davy Landman, Alexander Serebrenik, Eric Bouwers and Jurgen J. Vinju. Empirical analysis 
  of the relationship between CC and SLOC in a large corpus of Java methods 
  and C functions Journal of Software: Evolution and Process. 2016. 
  http://homepages.cwi.nl/~jurgenv/papers/JSEP-2015.pdf)
  
- what if you separate out the test sources?

Tips: 
- the AST data type can be found in module lang::java::m3::AST
- use visit to quickly find methods in Declaration ASTs
- compute McCabe by matching on AST nodes

Sanity checks
- write tests to check your implementation of McCabe

Bonus
- write visualization using vis::Figure and vis::Render to render a histogram.

*/


set[Declaration] jpacmanASTs() = createAstsFromEclipseProject(|project://jpacman-framework|, true); 

alias CC = rel[loc method, int cc];

CC cc(set[Declaration] decls) {
    CC result = {};
    visit (decls) {
        case m:\method(_, _, _, _, Statement body): {
            result[m.src] = calculateComplexity(body);
        }
    }
    return result;
}

int calculateComplexity(Statement body) {
    int count = 1;
    visit (body) {
        case s:\if(_, _, _): count += 1; // if with else
        case s:\if(_, _): count += 1; // if without else
        case s:\case(_): count += 1; // switch itself doesn't count. only case count matters
        case s:\foreach(_, _, _): count += 1; // foreach
        case s:\for(_, _, _, _): count += 1; // for with condition
        case s:\for(_, _, _): count += 1; // for without condition
        case s:\catch(_, _): count += 1; // try catch
        case s:\while(_, _): count += 1; // while
        case s:\do(_, _): count += 1; // do while
        case s:\infix(_, "||", _): count += 1; // || operator
        case s:\infix(_, "&&", _): count += 1; // && operator
        case s:\conditional(_, _, _): count += 1; // ? : operators
        
    }
    
    return count;
}

alias CCDist = map[int cc, int freq];

CCDist ccDist(CC cc) {
    // to be done
}

void answers() {
	int complexityAll = 0;
	int methodsComplexity = 0;
	loc file;
	CC c = cc(jpacmanASTs());
	for(<loc l, int n> <- c) {

		if(n > methodsComplexity) {
			methodsComplexity = n;
			file = l;
		}
		complexityAll+=n;
	}
	println("Project omplexity: ");
	println(complexityAll);
	println("The most complex method is located here: ");
	println(file);
	println("And its complexity is: ");
	println(methodsComplexity);
	
}


rel[str methodName, int cc] testResults() {
	rel[str methodName, int cc] result = {};

	visit({ createAstFromFile(|project://sqat-analysis/src/sqat/series1/A2_test.java|, true) }) {
		case m: \method(_, name, _, _, body):
			result += <name, calculateComplexity(body)>;
	}

	return result;
}

test bool ifTest() = testResults()["if_func"] == {2};
test bool ifElseTest() = testResults()["if_else"] == {2};
test bool switchCaseTest() = testResults()["switch_case"] == {4};
test bool forLoopTest() = testResults()["for_loop"] == {2};
test bool doWhileLoopTest() = testResults()["do_while_loop"] == {2};
test bool andInfixTest() = testResults()["and_infix"] == {3};
test bool orInfixTest() = testResults()["or_infix"] == {3};
test bool ternaryOperatorTest() = testResults()["ternary_operator"] == {2};
test bool tryCatchTest() = testResults()["try_catch"] == {2};


