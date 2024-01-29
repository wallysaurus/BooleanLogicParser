abstract class Node<T> (val value: T) {
    var left: Node<*>? = null
    var right: Node<*>? = null
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

    fun grabVariables(expr: String, operators: List<OperatorType> = OperatorType.entries) : List<String> =
        Regex("[^()${operators.joinToString("") { it.char.toString() }}]+").findAll(expr).map { it.value }.toList().distinct()

    fun validateExpression(translated: String) : Boolean {
        var counter = 0
        translated.forEach {
            if (it == '(') counter++
            else if (it == ')') {
                counter--
                if (counter < 0) throw ParseException("Parenthesis Abuse.")
            }
        }
        if (counter != 0) throw ParseException("Parenthesis Abuse.")
        return true
    }

    fun buildTree(expr: String) : Node<*> {
        println("Attempting to parse: $expr")

        // Grabs the range of content within the lowest-scoped parenthesis bodies.
        fun grabSection(expr: String): List<IntRange> {
            val mutableList: MutableList<IntRange> = mutableListOf()
            var startIndex = -1
            var counter = 0

            expr.forEachIndexed { index, char ->
                when (char) {
                    '(' -> {
                        if (startIndex == -1) startIndex = index + 1
                        counter++
                    }
                    ')' -> {
                        counter--
                        if (counter == 0 && startIndex != -1) {
                            mutableList.add(startIndex..< index)
                            startIndex = -1
                        }
                    }
                }
            }
            return mutableList
        }

        // Yank out all nested parenthesis.
        val sections = grabSection(expr).map { expr.substring(it) }.toList()
        var lowestScope = expr
        sections.forEach { lowestScope = lowestScope.replace("($it)", "") }

        // Find the first operator present in order of operations.
        var operatorIndex = -1
        listOf('≠', '=', '→', '⊕', '∨', '∧', '¬').forEach { operator ->
            if (operatorIndex == -1) operatorIndex = lowestScope.indexOfFirst { it == operator }
        }

        lateinit var node : Node<*>

        // Check if the scope holds an Operand or an Operator.
        if (operatorIndex != -1) {
            if (grabVariables(lowestScope).size != 1) throw ParseException("Two variables cannot be placed simultaneously without a separating operator [$lowestScope]")
            else node = Operand(lowestScope)
        } else {
            node = Operator(
                OperatorType.entries.find { lowestScope[operatorIndex] == it.char }
                    ?: throw ParseException("Was unable to associate an operator character with its enum: [${lowestScope[operatorIndex]}]")
            )

            val rawOperatorIndex = expr.indexOfFirst { it == lowestScope[operatorIndex] }

            // Check to make sure the expression isn't incomplete, i.e. [charlie AND] or [OR charlie]
            if (lowestScope.getOrNull(operatorIndex-1) == null || lowestScope.getOrNull(operatorIndex+1) == null) {
                // The NOT operator is the only exception to the above rule, provided that the operator precedes the expression.
                if (lowestScope[operatorIndex] == OperatorType.NOT.char && lowestScope.getOrNull(operatorIndex-1) == null) {
                    node.left = buildTree(expr.substring(0..< rawOperatorIndex))
                } else throw ParseException("Incomplete expression [${lowestScope}]")
            } else {
                node.left = buildTree(expr.substring(0..< rawOperatorIndex))
                node.right = buildTree(expr.substring(rawOperatorIndex+1 ..< expr.length))
            }
        }

        return node

    }

}

enum class OperatorType(val char: Char) { AND('∧'), OR('∨'), NOT('¬'), XOR('⊕'), EQUALS('='), NEQUALS('≠'), IMPLY('→'), }
class ParseException(message: String) : Exception(message)

fun main() {
    val expression = "((P OR (Q AND R)) AND NOT((S AND T) OR (U OR V)))"
    val translated = OperatorType.entries.fold(expression) { alias, operator -> alias.replace(operator.toString(), operator.char.toString()) }
        .replace("!=", OperatorType.NEQUALS.char.toString())
        .replace(" ", "")
    val variables = Parser.grabVariables(translated)

    Parser.validateExpression(expression)

    println("original: $expression")
    println("translated: $translated")
    println("variables: $variables")
    println(Parser.buildTree(translated).process(
        mapOf(
            "P" to true,
            "Q" to false,
            "R" to true,
            "S" to true,
            "T" to false,
            "U" to true,
            "V" to true
        )
    ))
}