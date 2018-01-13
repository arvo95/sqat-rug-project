module sqat::series1::A2_McCabe

import lang::java::jdt::m3::AST;
import analysis::statistics::Correlation;
import IO;


/*

Construct a distribution of method cylcomatic complexity. 
(that is: a map[int, int] where the key is the McCabe complexity, and the value the frequency it occurs)


Questions:
- which method has the highest complexity (use the @src annotation to get a method's location)

- how does pacman fare w.r.t. the SIG maintainability McCabe thresholds?

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
            result[m.src] = calculateCC(body);
        }
    }
    return result;
}

int calculateCC(Statement body) {
    int count = 1;
    visit (body) {
        case s:\if(_, _): count += 1; // if, no else
        case s:\if(_, _, _): count += 1; // if with else
        case s:\case(_): count += 1; // switch itself doesn't count
        case s:\for(_, _, _, _): count += 1; // for with condition
        case s:\for(_, _, _): count += 1; // for without condition
        case s:\foreach(_, _, _): count += 1; // foreach
        case s:\while(_, _): count += 1; // while
        case s:\do(_, _): count += 1; // do while
        case s:\infix(_, "&&", _): count += 1; // && operator
        case s:\infix(_, "||", _): count += 1; // || operator
        case s:\conditional(_, _, _): count += 1; // ? : operators
        case s:\catch(_, _): count += 1; // try catch
    }
    
    return count;
}

alias CCDist = map[int cc, int freq];

CCDist ccDist(CC cc) {
    return distribution(cc);
}

