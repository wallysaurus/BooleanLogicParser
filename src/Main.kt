object Main {

    fun case(a: Boolean, b: Boolean, expr: String): String {
        val raw = expr.replace(Regex("/[()]/"), "")
        if (raw in listOf("TRUE", "FALSE")) return raw

        val left = leftmostParenthesis(expr)
        val right = correspondingRight(expr, left)

        // grab the contents within the innermost parenthesis, excluding the parenthesis.
        val section = expr.subSequence((left ?: -1) + 1..<(right ?: expr.length)).toString()
        // use the operate() function to determine the result of the operation, and plug the value back into the equation.
        try {
            val result = expr.replaceRange((left ?: -1), (right ?: expr.length), operate(section, a, b))
            return case(a, b, result)
        } catch (error: Error) {
            println(expr)
        }
        // recurse over the expression until it has been reduced to a singular value. (i.e. "T" or "(T)")
        return case(a, b, "poo")
    }

    private fun leftmostParenthesis(expr: String): Int? {
        return (expr.length - 1 downTo 0).firstOrNull { expr[it] == '(' }
    }

    private fun correspondingRight(expr: String, left: Int?): Int? {
        if (left == null) return null
        return (left..<expr.length).firstOrNull { expr[it] == ')' }
    }

    // takes a pair of two values and an operator and returns the result of the operation.
    // the two values are decided by order of operations in Main.case()
    private fun operate(basicExpression: String, a: Boolean, b: Boolean): String {
        with(basicExpression) {
            return if (when {
                this.contains("∧") -> { a && b }
                this.contains("∨") -> { a || b }
                this.contains("⊕") -> { a != b } // if a does not equal b, one value has to be true and one has to be false.
                this.contains("=") -> { a == b }
                this.contains("->") -> { !a || b }
                else -> { false }
            }) "T" else "F"
        }
    }
}

fun main() {
    println(Main.case(a = true, b = false, expr = "(a+b+(a*b))"))
}