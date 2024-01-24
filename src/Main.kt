sealed class Node<T>(val value: T) {
    var left: Node<T>? = null
    var right: Node<T>? = null
    abstract fun process(variables: Map<String, Boolean>): Boolean
}

class Operator(operator: OperatorType) : Node<OperatorType>(operator) {
    override fun process(variables: Map<String, Boolean>): Boolean {
        return when (value) {
            OperatorType.AND -> (left?.process(variables) == true) && (right?.process(variables) == true)
            OperatorType.OR -> (left?.process(variables) == true) || (right?.process(variables) == true)
            OperatorType.XOR, // XOR and NOT EQUALS compute the same way, we just keep them separate for the symbols.
            OperatorType.NEQUALS -> (left?.process(variables) == true) != (right?.process(variables) == true)
            OperatorType.EQUALS -> (left?.process(variables) == true) == (right?.process(variables) == true)
            OperatorType.IMPLY -> !(left?.process(variables) ?: false) || (right?.process(variables) == true)
            OperatorType.NOT -> (right?.process(variables)?.not() == true)
        }
    }
}

class Operand(variable: String) : Node<String>(variable) {
    override fun process(variables: Map<String, Boolean>): Boolean {
        return variables[value] ?: throw ParseException("Variable $value not found in provided variables: (${variables.values.joinToString(", ")}).")
    }
}

object Parser {

    fun convert(expr: String, whitespace: Boolean = false) : String {
        return OperatorType.entries.fold(expr) { alias, operator -> alias.replace(operator.toString(), operator.char.toString()) }
            .replace("!=", OperatorType.NEQUALS.char.toString())
            .replace(" ", if (whitespace) " " else "" )
    }

    // this doesn't check the 7-bit ascii table or wtv, but honestly, who has time for that
    fun identifyVariables(expr: String) : List<String> {
        return Regex("[^()${OperatorType.entries.joinToString("") { it.char.toString() }}]+")
            .findAll(expr).map { it.value }.toList()
    }

    /*
    Error cases to be validated:
      1. Two variables seperated by whitespace ["alpha beta OR charlie"]
      2. Two variables seperated by NOT (can't be a standalone operator) ["alpha ¬beta"]
      3. Parenthesis abuse:
         - Expression contains backwards facing parenthesis ["(alpha AND beta))("]
         - Expression contains unequal amount of each parenthesis ["((alpha AND beta)"]
         - Expression contains empty parenthesis ["(alpha AND beta)()"]
         - Incomplete expression within parenthesis ["alpha (AND beta)"]
         3 and 4 will be checked later.
    */
    fun validateExpression(converted: String, wsConverted: String, variables: List<String>) {
        // Two variables seperated by whitespace:
        val wsVariables = Regex("[^() ${OperatorType.entries.joinToString("") { it.char.toString() }}]+")
            .findAll(wsConverted).map { it.value }.toList()
        if (wsVariables != variables) throw ParseException("All variables must be seperated by an operator.")

        // Two variables seperated by NOT:
        val notVariables = Regex("[^() ${OperatorType.entries.filter{it!=OperatorType.NOT}.joinToString("") { it.char.toString() }}]+")
            .findAll(wsConverted).map { it.value }.toList()
        if (variables != notVariables) throw ParseException("The NOT operator does not function as a standalone operator.")

        // Parenthesis abuse:
        var counter = 0
        converted.forEach {
            if (it == '(') counter++
            else if (it == ')') {
                counter--
                if (counter < 0) throw ParseException("Parenthesis Abuse.")
            }
        }
        if (counter != 0) throw ParseException("Parenthesis Abuse.")
    }

    fun <T> buildTree(expr: String, variables: Map<String, Boolean>) : Node<T> {
        val children : MutableList<String> = mutableListOf()

        // Grabs the index range within the first found parenthesis body.
        fun grabSection(expr: String) : IntRange {
            var counter = 0
            val start = expr.indexOfFirst { it == '(' }.coerceAtLeast(0)
            for (i in start+1..< expr.length) {
                if (expr[i] == '(') counter++
                else if (expr[i] == ')') {
                    counter --
                    if (counter == 0) return start+1..<i
                }
            }
            return start..<expr.length
        }

        // Takes a base expression (no nested parenthesis) and builds its node tree.
        fun <T> orderOfOp(expr: String) : Node<T> {
            val orderOfOperations : List<List<Char>> = listOf(
                listOf(OperatorType.NOT.char),
                listOf(OperatorType.AND.char),
                listOf(OperatorType.OR.char, OperatorType.XOR.char, OperatorType.IMPLY.char),
                listOf(OperatorType.EQUALS.char, OperatorType.NEQUALS.char)
            )


        }

        /*
            throw an exception if the parenthesis is empty,
            begin building the tree if there are no parenthesis present,
            or recurse this function if there are.
        */
        val section = grabSection(expr)
        if (section.isEmpty()) throw ParseException("Empty parenthesis body found.")
        else if (section == 1..<expr.length) {
            return orderOfOp(expr.substring(section))
        } else {
            var mutable = expr
            do {
                var s = grabSection(mutable)
                children.add(mutable.substring(s))
                mutable.removeRange(s)
            } while (grabSection(mutable) != 1..<mutable.length)
            children.forEach {  }
        }

        // figure out how to make the recursion fit in with the bv

    }

}

enum class OperatorType(val char: Char) { AND('∧'), OR('∨'), NOT('¬'), XOR('⊕'), EQUALS('='), NEQUALS('≠'), IMPLY('→'), }
class ParseException(message: String) : Exception(message)

fun main() {
    val expression = "alpha AND beta OR charlie"
    val converted = Parser.convert(expression)
    val variables = Parser.identifyVariables(converted)
    println("original: $expression")
    println("converted: $converted")
    println("variables: $variables")
    Parser.validateExpression(converted, Parser.convert(expression, true), variables)
}