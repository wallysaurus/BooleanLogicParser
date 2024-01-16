object Main {

    fun case(a: Boolean, b: Boolean, expr: String) : String {
        val raw = expr.replace(Regex("/[()]/"), "")
        if (raw in listOf("TRUE", "FALSE")) return raw

        val mutableExpr = expr
            .replace("a", if (a) "TRUE" else "FALSE")
            .replace("b", if (b) "TRUE" else "FALSE")
        val left = leftmostParenthesis(mutableExpr)
        val right = correspondingRight(mutableExpr, left)

        println("expr: $expr")
        println("mutableExpr: $mutableExpr")
        println("leftIndex: $left, rightIndex: $right")

        val section = mutableExpr.subSequence((left ?: -1)+1..<(right ?: expr.length)-1).toString()
        return section
    }

    private fun leftmostParenthesis(expr: String) : Int? {
        return (expr.length-1 downTo 0).firstOrNull { expr[it] == '(' }
    }

    private fun correspondingRight(expr: String, left: Int?) : Int? {
        if (left == null) return null
        return (left ..< expr.length).firstOrNull { expr[it] == ')' }
    }

}

fun main() {
    println(Main.case(a = true, b = false, expr = "(a+b+(a*b))"))
}