import java.io.File
import kotlin.math.pow
import kotlin.random.Random

// randomly generate a matrix of shape [height, width]
fun randMatrix(height: Int, width: Int): List<List<Float>> {
    assert(height > 0)
    assert(width > 0)

    // expRange is exclusive
    fun randFloatValidBits(expRange: Int): Float {
        val floatExpBits = 8
        assert(expRange in 1..(1 shl floatExpBits))
        val f = Random.nextFloat() + 1 // \in [1, 2)
        val sign = if (Random.nextBoolean()) -1 else 1
        val exp = Random.nextInt(-expRange + 1, expRange)
        return sign * f * 2.0f.pow(exp)
    }

    return List(height) {
        List(width) {
            randFloatValidBits(expRange = 50) // ensures no 'inf' will be produced
        }
    }
}

// each sublist of matrix/kernel must have the same size, otherwise the assertions will fail
fun convolution(matrix: List<List<Float>>, kernel: List<List<Float>>): List<List<Float>> {
    val matrixHeight = matrix.size
    val matrixWidth = matrix.first().size
    val kernelHeight = kernel.size
    val kernelWidth = kernel.first().size
    assert(kernelHeight > 0)
    assert(kernelWidth > 0)
    assert(matrixHeight >= kernelHeight)
    assert(matrixWidth >= kernelWidth)
    assert(matrix.all { it.size == matrixWidth })
    assert(kernel.all { it.size == kernelWidth })
    return List(matrixHeight - kernelHeight + 1) { h ->
        List(matrixWidth - kernelWidth + 1) { w ->
            (0 until kernelHeight).map { i ->
                (0 until kernelWidth).map { j ->
                    matrix[i + h][j + w] * kernel[i][j]
                }.sum()
            }.sum()
        }
    }
}

// output: (matrix, kernel, convolution_output)
fun generateOneTestCase(
    matrixHeight: Int, matrixWidth: Int, kernelHeight: Int, kernelWidth: Int
): Triple<List<List<Float>>, List<List<Float>>, List<List<Float>>> {
    assert(matrixHeight > 0)
    assert(matrixWidth > 0)
    assert(kernelHeight > 0)
    assert(kernelWidth > 0)
    val matrix = randMatrix(matrixHeight, matrixWidth)
    val kernel = randMatrix(kernelHeight, kernelWidth)
    val output = convolution(matrix, kernel)
    return Triple(matrix, kernel, output)
}

fun generateTestCases(
    matrixShapeList: List<Pair<Int, Int>>,
    kernelShapeList: List<Pair<Int, Int>>
): List<Triple<List<List<Float>>, List<List<Float>>, List<List<Float>>>> =
    matrixShapeList.flatMap { (mh, mw) ->
        kernelShapeList.filter { (kh, kw) -> mh >= kh && mw >= kw }
            .map { (kh, kw) ->
                generateOneTestCase(mh, mw, kh, kw)
            }
    }

fun printMatrix(matrix: List<List<Float>>, path: String) =
    File(path).printWriter().use { f ->
        matrix.forEach { row ->
            f.println(row.map { it.toString() }.reduce { a, b -> "$a $b" })
        }
    }

fun printOneTestCase(matrices: Triple<List<List<Float>>, List<List<Float>>, List<List<Float>>>, dirPath: String) {
    val (matrix, kernel, output) = matrices
    val matrixHeight = matrix.size
    val matrixWidth = matrix.first().size
    val kernelHeight = kernel.size
    val kernelWidth = kernel.first().size
    val subDirName = "${matrixHeight}x${matrixWidth}_${kernelHeight}x${kernelWidth}"
    val subDirPath = "$dirPath/$subDirName"
    File(subDirPath).mkdir()
    printMatrix(matrix, "$subDirPath/matrix.txt")
    printMatrix(kernel, "$subDirPath/kernel.txt")
    printMatrix(output, "$subDirPath/output.txt")
    println("$subDirName Finished")
}

fun printTestCases(tripleList: List<Triple<List<List<Float>>, List<List<Float>>, List<List<Float>>>>, dirPath: String) =
    tripleList.forEach { printOneTestCase(it, dirPath) }

fun main() {
    val beginTime = System.currentTimeMillis()
    val testCasesPath = "test_cases"
    File(testCasesPath).mkdir()
    val tripleList = generateTestCases(
        listOf(
            Pair(32, 32), Pair(64, 64), Pair(128, 128), Pair(256, 256), Pair(512, 512), Pair(1024, 1024)
        ),
        listOf(
            Pair(16, 16), Pair(64, 64)
        )
    )
    printTestCases(tripleList, testCasesPath)
    val endTime = System.currentTimeMillis()
    println("total time: ${endTime - beginTime}ms")
}
