package com.gregghz.emacs

import java.util.*
import foo.Bar
import bar.Bar as bBar

// a single line comment

/**
 * a multiline comment
 */

fun sum(a: Int, b: Int): Int {
    return a + b
}

fun sum(a: Int, b: Int) = a + b

fun printSum(a: Int, b: Int): Unit {
    print(a + b)
}

fun printSum(a: Int, b: Int) {
    print(a + b)
}

val a: Int = 1
val b = 1
val c: Int
c = 1

var x = 5
x += 1

fun main(args: Array<String>) {
    if (args.size == 0) return

    print("First argument: ${args[0]}")
}

fun max(a: Int, b: Int): Int {
    if (a > b)
    return a
    else
    return b
}

fun max(a: Int, b: Int) = if (a > b) a else b

fun parseInt(str: String): Int? {

}

val x = parseInt(args[0])
val y = parseInt(args[1])
if (x != null && y != null) {
    print(x * y)
}

fun getStringLength(obj: Any): Int? {
    if (obj is String) {
        return obj.length
    }

    return null
}

fun main(args: Array<String>) {
    for (arg in args)
    print(arg)
}

for (i in args.indices)
print(args[i])

fun main(args: Array<String>) {
    var i = 0
    while (i < args.size)
    print(args[i++])
}

fun cases(obj: Any) {
    when (obj) {
        1          -> print("One")
        "Hello"    -> print("Greeting")
        is Long    -> print("Long")
        !is String -> print("Not a string")
        else       -> print("Unknown")
    }
}

if (x in 1..y-1)
print("OK")

if (x !in 0..array.lastIndex)
print("Out")

for (x in 1..5)
print(x)

for (name in names)
println(name)

if (text in names) // names.contains(text) is called
print("Yes")

names.filter { it.startsWith("A") }
    .sortedBy { it }
    .map { it.toUpperCase() }
    .forEach { print(it) }

fun f() {
    things.f()
        .g()
        .h()
}

data class Customer(val name: String, val email: String)
val positives = list.filter { x -> x > 0 }
val positives = list.filter { it > 0 }
println("Name $name")
for ((k, v) in map) {
    println("$k -> $v")
}

val list = listOf("a", "b", "c")

println(map["key"])
map["key"] = value

val p: String by lazy {
    // compute the string
}

object Resource {
    val name = "Name"
}

val files = File("Test").listFiles()

println(files?.size)

val files = File("Test").listFiles()

println(files?.size ?: "empty")

val email = data["email"] ?: throw IllegalStateException("Email is missing!")

data?.let {
    // execute this block if not null
}

fun transform(color: String): Int {
    return when (color) {
        "Red" -> 0
        "Green" -> 1
        "Blue" -> 2
        else -> throw IllegalArgumentException("Invalid color param value")
    }
}

fun test() {
    val result = try {
        count()
    } catch (e: ArithmeticException) {
        throw IllegalStateException(e)
    }

    // Working with result
}

fun foo(param: Int) {
    val result = if (param == 1) {
        "one"
    } else if (param == 2) {
        "two"
    } else {
        "three"
    }
}

fun arrayOfMinusOnes(size: Int): IntArray {
    return IntArray(size).apply { fill(-1) }
}

class Turtle {
    fun penDown()
    fun penUp()
    fun turn(degrees: Double)
    fun forward(pixels: Double)
}

val myTurtle = Turtle()
with(myTurtle) {
    //draw a 100 pix square
    penDown()
    for(i in 1..4) {
        forward(100.0)
        turn(90.0)
    }
    penUp()
}

val stream = Files.newInputStream(Paths.get("/some/file.txt"))
stream.buffered().reader().use {
    reader -> println(reader.readText())
}

inline fun <reified T: Any> Gson.fromJson(json): T = this.fromJson(json, T::class.java)

loop@ for (i in 1..100) {
    for (j in 1..100) {
        if (x)
        break@loop
    }
}

fun foo() {
    ints.forEach lit@ {
        if (it == 0) return@lit
        print(it)
    }
}

class Invoice {
}

class Empty

class Person constructor(firstName: String) {
}

class Person(firstName: String) {
}

class Customer(name: String) {
    init {
        logger.info("Customer initialized with value ${name}")
    }
}

class Customer(name: String) {
    val customerKey = name.toUpperCase()
}

class Person(val firstName: String, val lastName: String, var age: Int) {
    // ...
}

class Customer public @Inject constructor(name: String) { }

class Person {
    constructor(parent: Person) {
        parent.children.add(this)
    }
}

class Person(val name: String) {
    constructor(name: String, parent: Person) : this(name) {
        parent.children.add(this)
    }
}

class DontCreateMe private constructor () {
}

val invoice = Invoice()

val customer = Customer("Joe Smith")

open class Base(p: Int)

class Derived(p: Int) : Base(p)

class MyView : View {
    constructor(ctx: Context) : super(ctx) {
    }

    constructor(ctx: Context, attrs: AttributeSet) : super(ctx, attrs) {
    }
}

open class Base {
    open fun v() {}
    fun nv() {}
}
class Derived() : Base() {
    override fun v() {}
}

open class AnotherDerived() : Base() {
    final override fun v() {}
}

open class Foo {
    open val x: Int get { }
}

class Bar1(override val x: Int) : Foo() {

}

open class A {
    open fun f() { print("A") }
    fun a() { print("a") }
}

interface B {
    fun f() { print("B") } // interface members are 'open' by default
    fun b() { print("b") }
}

class C() : A(), B {
    // The compiler requires f() to be overridden:
    override fun f() {
        super<A>.f() // call to A.f()
        super<B>.f() // call to B.f()
    }
}

open class Base {
    open fun f() {}
}

abstract class Derived : Base() {
    override abstract fun f()
}

sealed class Expr {
    class Const(val number: Double) : Expr()
    class Sum(val e1: Expr, val e2: Expr) : Expr()
    object NotANumber : Expr()
}

fun eval(expr: Expr): Double = when(expr) {
    is Expr.Const -> expr.number
    is Expr.Sum -> eval(expr.e1) + eval(expr.e2)
    Expr.NotANumber -> Double.NaN
    // the `else` clause is not required because we've covered all the cases
}

var stringRepresentation: String
get() = this.toString()
set(value) {
    setDataFromString(value) // parses the string and assigns values to other properties
}

var setterVisibility: String = "abc"
private set // the setter is private and has the default implementation

var setterWithAnnotation: Any? = null
@Inject set // annotate the setter with Inject

var counter = 0 // the initializer value is written directly to the backing field
set(value) {
    if (value >= 0)
    field = value
}

val isEmpty: Boolean
get() = this.size == 0

const val SUBSYSTEM_DEPRECATED: String = "This subsystem is deprecated"

@Deprecated(SUBSYSTEM_DEPRECATED) fun foo() { ... }

public class MyTest {
    lateinit var subject: TestSubject

    @SetUp fun setup() {
        subject = TestSubject()
    }

    @Test fun test() {
        subject.method()  // dereference directly
    }
}

interface MyInterface {
    fun bar()
    fun foo() {
        // optional body
    }
}

class Child : MyInterface {
    override fun bar() {
        // body
    }
}

interface MyInterface {
    val property: Int // abstract

    val propertyWithImplementation: String
    get() = "foo"

    fun foo() {
        print(property)
    }
}

class Child : MyInterface {
    override val property: Int = 29
}

interface A {
    fun foo() { print("A") }
    fun bar()
}

interface B {
    fun foo() { print("B") }
    fun bar() { print("bar") }
}

class C : A {
    override fun bar() { print("bar") }
}

class D : A, B {
    override fun foo() {
        super<A>.foo()
        super<B>.foo()
    }
}

private fun foo() {} // visible inside example.kt

public var bar: Int = 5 // property is visible everywhere
private set         // setter is visible only in example.kt

internal val baz = 6    // visible inside the same module

open class Outer {
    private val a = 1
    protected val b = 2
    internal val c = 3
    val d = 4  // public by default

    protected class Nested {
        public val e: Int = 5
    }
}

fun MutableList<Int>.swap(index1: Int, index2: Int) {
    val tmp = this[index1] // 'this' corresponds to the list
    this[index1] = this[index2]
    this[index2] = tmp
}

fun <T> MutableList<T>.swap(index1: Int, index2: Int) {
    val tmp = this[index1] // 'this' corresponds to the list
    this[index1] = this[index2]
    this[index2] = tmp
}

fun Any?.toString(): String {
    if (this == null) return "null"
    // after the null check, 'this' is autocast to a non-null type, so the toString() below
    // resolves to the member function of the Any class
    return toString()
}

val <T> List<T>.lastIndex: Int
get() = size - 1

class MyClass {
    companion object { }  // will be called "Companion"
}

fun MyClass.Companion.foo() {
    // ...
}

open class D {
}

class D1 : D() {
}

open class C {
    open fun D.foo() {
        println("D.foo in C")
    }

    open fun D1.foo() {
        println("D1.foo in C")
    }

    fun caller(d: D) {
        d.foo()   // call the extension function
    }
}

class C1 : C() {
    override fun D.foo() {
        println("D.foo in C1")
    }

    override fun D1.foo() {
        println("D1.foo in C1")
    }
}

val jack = User(name = "Jack", age = 1)
val olderJack = jack.copy(age = 2)

val jane = User("Jane", 35)
val (name, age) = jane
println("$name, $age years of age") // prints "Jane, 35 years of age"

class Box<T>(t: T) {
    var value = t
}

val box: Box<Int> = Box<Int>(1)

abstract class Source<out T> {
    abstract fun nextT(): T
}

abstract class Comparable<in T> {
    abstract fun compareTo(other: T): Int
}

fun copy(from: Array<out Any>, to: Array<Any>) {
    // ...
}

fun fill(dest: Array<in String>, value: String) {
    // ...
}

fun <T : Comparable<T>> sort(list: List<T>) {
    // ...
}

fun <T> cloneWhenGreater(list: List<T>, threshold: T): List<T>
where T : Comparable,
T : Cloneable {
    return list.filter { it > threshold }.map { it.clone() }
}

enum class Direction {
    NORTH, SOUTH, WEST, EAST
}

enum class Color(val rgb: Int) {
    RED(0xFF0000),
    GREEN(0x00FF00),
    BLUE(0x0000FF)
}

enum class ProtocolState {
    WAITING {
        override fun signal() = TALKING
    },

    TALKING {
        override fun signal() = WAITING
    };

    abstract fun signal(): ProtocolState
}

window.addMouseListener(object : MouseAdapter() {
    override fun mouseClicked(e: MouseEvent) {
        // ...
    }

    override fun mouseEntered(e: MouseEvent) {
        // ...
    }
})

val adHoc = object {
    var x: Int = 0
    var y: Int = 0
}
print(adHoc.x + adHoc.y)

fun countClicks(window: JComponent) {
    var clickCount = 0
    var enterCount = 0

    window.addMouseListener(object : MouseAdapter() {
        override fun mouseClicked(e: MouseEvent) {
            clickCount++
        }

        override fun mouseEntered(e: MouseEvent) {
            enterCount++
        }
    })
    // ...
}

object DefaultListener : MouseAdapter() {
    override fun mouseClicked(e: MouseEvent) {
        // ...
    }

    override fun mouseEntered(e: MouseEvent) {
        // ...
    }
}

class MyClass {
    companion object Factory {
        fun create(): MyClass = MyClass()
    }
}

interface Factory<T> {
    fun create(): T
}


class MyClass {
    companion object : Factory<MyClass> {
        override fun create(): MyClass = MyClass()
    }
}

infix fun Int.shl(x: Int): Int {

}

fun <T> asList(vararg ts: T): List<T> {
    val result = ArrayList<T>()
    for (t in ts) // ts is an Array
    result.add(t)
    return result
}

tailrec fun findFixPoint(x: Double = 1.0): Double
= if (x == Math.cos(x)) x else findFixPoint(Math.cos(x))

fun <T> lock(lock: Lock, body: () -> T): T {
    lock.lock()
    try {
        return body()
    }
    finally {
        lock.unlock()
    }
}

val result = lock(lock, ::toBeSynchronized)

fun <T, R> List<T>.map(transform: (T) -> R): List<R> {
    val result = arrayListOf<R>()
    for (item in this)
    result.add(transform(item))
    return result
}

val doubled = ints.map { it -> it * 2 }

strings.filter { it.length == 5 }.sortBy { it }.map { it.toUpperCase() }

max(strings, { a, b -> a.length < b.length })

fun <T> max(collection: Collection<T>, less: (T, T) -> Boolean): T? {
    var max: T? = null
    for (it in collection)
    if (max == null || less(max, it))
    max = it
    return max
}

val sum: (Int, Int) -> Int = { x, y -> x + y }

fun(x: Int, y: Int): Int = x + y

fun(x: Int, y: Int): Int {
    return x + y
}

ints.filter(fun(item) = item > 0)

var sum = 0
ints.filter { it > 0 }.forEach {
    sum += it
}
print(sum)

inline fun foo(inlined: () -> Unit, noinline notInlined: () -> Unit) {
    // ...
}

fun <T> TreeNode.findParentOfType(clazz: Class<T>): T? {
    var p = parent
    while (p != null && !clazz.isInstance(p)) {
        p = p?.parent
    }
    @Suppress("UNCHECKED_CAST")
    return p as T
}

inline fun <reified T> TreeNode.findParentOfType(): T? {
    var p = parent
    while (p != null && p !is T) {
        p = p?.parent
    }
    return p as T
}

class Test {
    fun f() {

    }
}

fun itpl() {
    print("$foo/bar");
    print("$`weird$! identifier`bar");
    print("${foo}bar");
    print("${`weird$! identifier`}bar");
}

