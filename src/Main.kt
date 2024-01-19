object Main {

    fun case(a: Boolean, b: Boolean, expr: String): String {
        val raw = expr.replace("(", "").replace(")", "")
        if (raw in listOf("T", "F")) return raw

        val left = leftmostParenthesis(expr)
        val right = correspondingRight(expr, left)
        println("left: $left, right: $right, expr: $expr")

        // grab the contents within the innermost parenthesis, excluding the parenthesis.
        val section = expr.subSequence((left ?: -1) + 1..<(right ?: expr.length)).toString()
        // use the operate() function to determine the result of the operation, and plug the value back into the equation.
        val result = expr.replaceRange((left ?: -1), (right ?: expr.length) + 1, operate(section, a, b))

        // TODO(handle expressions with 3 or more parts + order of operations.)

        return case(a, b, result)
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
                this.contains("∧"),
                this.contains("AND") -> { a && b }
                this.contains("∨"),
                this.contains("OR") -> { a || b }
                this.contains("⊕"),
                this.contains("XOR") -> { a != b } // if a does not equal b, one value has to be true and one has to be false.
                this.contains("="),
                this.contains("EQUALS") -> { a == b }
                this.contains("->"),
                this.contains("IMPLIES") -> { !a || b }
                else -> { false }
            }) "T" else "F"
        }
    }
}

fun main() {
    println(Main.case(a = true, b = false, expr = "(aORbIMPLIESb)"))
}