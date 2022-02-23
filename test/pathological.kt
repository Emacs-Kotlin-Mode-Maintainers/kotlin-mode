#!/usr/bin/kotlinc -script

// fileAnnotation
@file
    :
    [
        A(a = 1) // No Comma here
        B
            .
            C
            .
            D // No Comma here
        E
    ]

@file
    :
    Foo
    .
    Bar<
        A // KNOWN_BUG
    > // KNOWN_BUG
    .
    Baz

@file
    :
    Foo(aaa)

// packageHeader
package foo // Line breaks are prohibited after "package".
    .bar // Line breaks are prohibited after dot.
    .baz

// importList, importHeader
import a // Line breaks are prohibited after "import".
    .b // Line breaks are prohibited after dot.
    .c
    .*

// importAlias
import a
    . b
    . c
    as d

// typeAlias
@A
public
    typealias
    A
    <
        B, // KNOWN_BUG_WHEN_TRUNCATED
        C // KNOWN_BUG_WHEN_TRUNCATED
    >
    =
    D

// classDeclaration

@A
public
    sealed
    class
    A
    <
        A, // KNOWN_BUG_WHEN_TRUNCATED
        B // KNOWN_BUG_WHEN_TRUNCATED
    >
    // primaryConstructor
    public
    constructor
    (
        // classParameters
        val
            x
            :
            Foo
            =
            foo,
        var
            y
            :
            Foo
            =
            foo
    )
    :
    // delegationSepcifiers
    Base
        by
        x +
        x,
    B,
    C,
    D
        .
        (Int)
        ->
        Int
    where
        @A
        A
            :
            A,
        B
            :
            B {

    public
        interface
        A {
        fun a(): Int
        fun b(): Int
    }

    // typeParameters
    data
        class
        Bar<
            @A // KNOWN_BUG_WHEN_TRUNCATED
            out
                A // KNOWN_BUG_WHEN_TRUNCATED
                :
                A,
            @A // KNOWN_BUG_WHEN_TRUNCATED
            in // KNOWN_BUG_WHEN_TRUNCATED
                A
                :
                A
        >
    { // brace on its own line
        fun a() {
        }
    }

    inner
        enum
        class
        A(val x: Int) {
        // enumClassBody
        A(1), B(1),
        C(1) {
            override fun a() = 2
        }, D(1) {
            override fun a() = 3
        },
        E(1);

        fun a(): Int = x
    }

    // typeConstraints
    public
        class
        Foo<
            A // KNOWN_BUG_WHEN_TRUNCATED
        >
        :
        X,
        Y
        where
            A
                :
                B,
            A
                :
                C {
    }

    public class Foo<A>: X, Y
        where A
                  :
                  B,
              A
                  :
                  C {
    }

    public class Foo<A>: X, Y where
        A
            :
            B,
        A
            :
            C {
    }

    public class Foo<A>: X, Y where A
                                        :
                                        B,
                                    A
                                        :
                                        C {
    }

    fun <
        A
    > Foo.foo()
        : A
        where
            A
                :
                B,
            A
                :
                C {
    }

    fun <
        A
    > Foo.foo()
        : A
        where
            A
                :
                B,
            A
                :
                C
        =
        A()

    val
        <
            A // KNOWN_BUG_WHEN_TRUNCATED
        >
        A
        .
        x
        where
            A
                :
                B,
            A
                :
                C
        =
        1

    val f = fun A.(): A
        where
            A:
                B,
            A:
                C {
    }

    class Foo<T> where T: A
                     , T: B
                     , T: C

    // anonymousInitializer
    init {
        a()
    }

    // companionObject
    public
        companion
        object
        A
        :
        B {
        fun foo() {
        }
    }

    // functionDeclaration
    public
        fun
        <
            A // KNOWN_BUG_WHEN_TRUNCATED
        >
        A
        .
        foo
        // functionValueParameters // KNOWN_BUG
        (
            a: Int = 1
        )
        :
        A
        where
            A : A {
        a()
    }

    fun
        foo(x)
        =
        x + 1

    // propertyDeclaration
    public
        val
        <
            A // KNOWN_BUG_WHEN_TRUNCATED
        >
        A
        .
        @A
        a
        :
        A
        where
            A
                :
                A
        =
        a
        public get() = 1
        public set(value) {
            foo(value)
        }

    // delegated property
    var
        x : Int
        by
        foo

    // multiVariableDeclaration
    public
        var
        (
            x: Int,
            y: Int
        )

    // getter/setter with default implementation
    var x = 1
        @A get
        @A set

    // objectDeclaration
    public
        object
        Foo
        :
        Bar {
        fun foo() {
            bar()
        }
    }

    // secondaryConstructor
    public
        constructor
        (
        )
        :
        this
        (
            1
        ) {
        a()
    }

    public
        constructor
        (
        )
        :
        super
        (
            1
        )

    // dynamic type
    var x: dynamic = 1

    // nullableType
    var
        x
        :
        A
        <
            X, // KNOWN_BUG_WHEN_TRUNCATED
            *, // KNOWN_BUG_WHEN_TRUNCATED
            out
                Y,
            in
                Z
        >
        .
        B
        .
        C
        <
            X // KNOWN_BUG_WHEN_TRUNCATED
        >
        ?
        ?
        ?
        =
        null

    var
        x
        :
        (
            Int
        )
        ?
        =
        null

    // functionType
    var
        f
        :
        // parehthesized nullable receiver type
        (
            A
        )
        ?
        .
        (
            Int
        )
        ->
        (
            Int
        )
        ->
        C

    // value class

    value
        class Foo { // KNOWN_BUG
    } // KNOWN_BUG
}


// statements
fun foo() {
    //explicit semicolons
    /* aaa */ a(); b();
    c();

    // annotation
    @A
    @B
        .D(aaa) @C
    @[
        A
        B
        C
    ]
    // label
    aaa@
    a()

    // forStatement
    for (
        @A
        a
            :
            A
        in
        aaa
    ) {
        a()
    }

    for
        (
            @A
            (x, y)
            in
            aaa
        )
    {
        a()
    }

    for (
        a in aaa
    )
        a()

    // whileStatement
    while (
        a()
    ) {
        a()
    }

    while
        (
            a()
        )
    {
        a()
    }

    while (
        a()
    )
        a()

    while (
        a()
    )
        ;

    // doWhileStatement
    do {
        a()
    } while (
        a()
    )

    do
    {
        a()
    }
    while
        (
            a()
        )

    do
        a() +
        b()
    while (a())

    do
    while (a())

    while (a())
        do
            do
            while(a())
        while(a())

    while(a())
        do
        while(a())

    // assignment
    (x, y) = // Line breaks are prohibited before assignment operators.
        a()

    aaa[x] =
        1

    a
        .
        b
        ?.
        c
        .
        d =
        e

    a
        .
        b +=
        1

    a
        .
        b -=
        1

    a
        .
        b *=
        1

    a
        .
        b %=
        1

    // expression
    // various operators
    val x =
        a
        ||
        b
        &&
        c

    val x =
        a == // Line breaks are prohibited before equality operators
        b

    val x =
        a !=
        b

    val x =
        a ===
        b

    val x =
        a !==
        g

    val x =
        a < // Line breaks are prohibited before comparison operators
        b

    val x =
        a >
        b

    val x =
        a <=
        b

    val x =
        a >=
        b

    val x =
        foo(a) in // Line breaks are prohibited before in/is operators
        b

    val x =
        a !in
        b

    when (a()) {
        1 -> a()
        // Line breaks are prohibited before in/is operators, So the following
        // line should not be indented.
        in aaa -> a()
        !in aaa -> a()
    }

    val x =
        a is
        b

    val x =
        a !is
        b

    when (a()) {
        1 -> a()
        // Line breaks are prohibited before in/is operators, So the following
        // line should not be indented.
        is X -> a()
        !is X -> a()
    }

    val x =
        a
        ?:
        b

    // infixFunctionCall
    val x =
        a shl // Line breaks are allowed after infix function.
        b // KNOWN_BUG

     var shl = 1 // KNOWN_BUG
     val x = shl shl shl
     shl < 100 && foo() // this is not a continuation of the previous line.

     var shl = 1
     val x = shl shl
         shl < 100 && foo() // this is a continuation of the previous line. // KNOWN_BUG

    // This is not a infix function call; line breaks are // KNOWN_BUG
    // prohibited before infix function.
    val x = // KNOWN_BUG
        a
    f (b) // So this line should not indented.

    val x =
        a .. // Line breaks are prohibited before range operator.
        b

    val x =
        a + // Line breaks are prohibited before additive operators.
        b -
        c

    a()
    +a() // So this line should not be indented.
    -a() // Nor should this line.

    val x =
        a * // Line breaks are prohibited before multiplicative operators.
        b /
        c %
        d

    val x =
        a
        as
        A
        as?
        B

    // prefixUnaryExpression
    val x =
        @a
        a@ // label
        +
        -
        ++
        a // KNOWN_BUG

    val x = // KNOWN_BUG
        --
        a // KNOWN_BUG

    val x = // KNOWN_BUG
        !
        a // KNOWN_BUG

    // postfixUnaryExpression // KNOWN_BUG
    val x = // KNOWN_BUG
        a++ // Line breaks are prohibited before postfix operators.

    val x =
        a
    ++ a // So this line should not be indented.

    val x =
        a--

    val x =
        a
    -- a // This line too.

    var shl = 1
    val x = shl shl shl ++
    shl < 100 && foo() // this is not a continuation of the previous line.

    var shl = 1
    val x = shl shl ++
        shl < 100 && foo() // this is a continuation of the previous line. // KNOWN_BUG

    val x = // KNOWN_BUG
        a!!

    val x = foo()!!
    foo() // this is not a continuation of the previous line.

    val x = !!
        foo() // this is a continuation of the previous line. // KNOWN_BUG

    val x = // KNOWN_BUG
        f< // Line breaks are prohibited before type arguments.
            A // KNOWN_BUG_WHEN_TRUNCATED
        >( // Line breaks are prohibited before function arguments.
            x
        )[ // Line breaks are prohibited before subscript.
            x
        ]
        .
        a
        .
        b

    // lambda arguments
    val x = f()
    {
        a()
    }

    val x = f()
    @A
    a@
    {
        a()
    }

    val x = f() @A a@ {
        a()
    }

    val x = f
    {
        a()
    }

    val x = f
    @A
    a@
    {
        a()
    }

    val x = f @A a@ {
        a()
    }

    val x = x
        .foo {
            a
        }
        .bar {
            a
        }

    val x = x.foo {
        a
    }.bar {
        a
    }

    val x =
        f // Line breaks are prohibited before function arguments.
    (1 + 1).also { print(it) } // So this line should not be indented.

    val x =
        a // Line breaks are prohibited before function arguments.
    [g()].forEach { print(it) } // So this line should not be indented.

    // parenthesizedExpression
    val x = (
        a()
    )

    // collectionLiteral
    @A(x = [
           1, 2, 3,
           /* aaa */ 4, 5, 6,
           7, 8, 9
       ])
    a()

    // CharacterLiteral
    val x =
        'a'

    val x =
        '\''

    val x =
        '"'

    // stringLiteral
    val x = "abc\"def${
        foo("abc")
    }ghi${
        "aaa\${ // dollar sign cannot be escaped
            1
        }bbb"
    }jkl"

    val x = """a
               "b"
               c${
                   foo("""a
                           b
                            c
                       """)
               }d
    e
    f${
        """aaa\${
            1
        }bbb"""
    }ghi"""

    val x =
        a("'")

    // lambdaLiteral
    val f: () -> Unit = {
        a()
        a()
        a()
    }

    val f: () -> Unit = { ->
        a()
        a()
        a()
    }

    val f: () -> Unit = {
        ->
        a()
        a()
        a()
    }

    val f: () -> Unit = { x,
                          @A
                          y
                              :
                              Int,
                          (
                              z1,
                              z2
                          )
                              :
                              AAA
                          ->
        a()
        a()
        a()
    }

    val f: () -> Unit = {
        x,
        y,
        z
        ->
        a()
        a()
        a()
    }

    val f: () -> Unit = {
        (x,
         y,
         z)
        ->
        a()
        a()
        a()
    }

    // functionLiteral

    val f =
        fun
        A
        .
        (
            x: Int
        )
        :
        AAA
        where
            A
                :
                A,
            B
                :
                B {
            a() // KNOWN_BUG
        } // KNOWN_BUG

    val f = fun
    {
        a()
    }

    // objectLiteral
    val object: A by a, B by b {
        fun foo() {
        }
    }

    val x =
        object
        :
        A,
        B
            by
            b,
        C {
            fun foo() {
            }
        }

    val x = object
    {
        fun foo() {
        }
    }

    // thisExpression
    val x =
        this

    val x =
        this@Foo

    // superExpression
    val x =
        super

    val x =
        super@Foo

    val x =
        super<
            Int // KNOWN_BUG_WHEN_TRUNCATED
        >@Foo

    // ifExpression
    val x = if (
        a +
            1
    ) {
        a()
    }

    val x = if
        (
            a +
                1
        )
    {
        a()
    }

    val x = if (
        a
    )
        a()

    val x = if (
        a
    )
        ;

    val x = if (
        a
    ) {
        a()
    } else {
        a()
    }

    val x = if
        (
            a
        )
    {
        a()
    }
    else
    {
        a()
    }

    val x = if (
        a
    )
        a()
    else
        a()

    val x = if (
        a
    )
        ;
    else
        ;

    val x = if (
        a
    )
    else
        ;

    val x =
        if (
            a
        ) {
            a()
        } else {
            a()
        }

    val x = if (foo) 1
    else 2 // should be indented?

    val x =
        if (foo) 1
        else 2

    val x = if (foo) 1 +
                         1
    else 2 +
             1

    val x = if (foo)
        if (bar)
            aaa() +
                1
        else
            if (baz)
                if (aaa) aaa else bbb +
                                      1
            else
                if (aaa) aaa else
                                 bbb +
                                     1
    else
        if (bar)
            if (aaa)
                if (bbb)
                    ccc +
                        1 +
                        2
                else
                    ccc +
                        1 +
                        2
            else
                ccc +
                    1 +
                    2
        else
            aaa() +
                1

    val x = if (foo)
        while (false) {
            aaa() +
                1
        }
    else
        while (false) {
            aaa() +
                1
        }

    val x = if (a)
        foo() +
            1
    else if (e)
        foo() +
            1
    else
        foo() +
            1

    val x = if (a) {
        foo() +
            1
    } else if (e) {
        foo() +
            1
    } else {
        foo() +
            1
    }

    val x = if (a) {
        foo() +
            1
    } else
          if (e) {
              foo() +
                  1
          } else {
              foo() +
                  1
          }

    val x = if (a)
    {
        foo() +
            1
    }
    else if (e)
    {
        foo() +
            1
    }
    else
    {
        foo() +
            1
    }

    val x = if (a) if (a) 1 +
                              1 else 1 else
                                           if (a) if (a) 1 +
                                                             1 else 1 else
                                                                          1

    // whenExpression

    val x = when (
        @A
        val
            a
            =
            a()
    ) {
        a(), b(),
        c(), d()
            ->
        {
            a()
        }

        a() -> {
            a()
        }

        in
            a()
            ->
        {
            a ()
        }

        is
            A
            ->
        {
        }

        a()
            ->
            if (x) {
                1
            } else {
                2
            }

        if (x)
            1
        else
            2
            ->
            if (x)
                1
            else
                2

        else
            ->
        {
            a()
        }
    }

    val x = when
        (
            a()
        )
    {
        a -> 1
    }

    // tryExpression
    val x = try {
        a()
    } catch(@A e: E) {
        a()
    } catch(@A e: E) {
        a()
    } finally {
        a()
    }

    val x =
        try {
            a()
        } catch(@A e: E) {
            a()
        } catch(@A e: E) {
            a()
        } finally {
            a()
        }

    val x = try
    {
        a()
    }
    catch
        (
            @A e: E
        )
    {
        a()
    }
    finally
    {
        a()
    }

    val x = try {
        a()
    } finally {
        a()
    }

    val x = try
    {
        a()
    }
    finally
    {
        a()
    }

    // jumpExpression
    val x =
        throw
        a()

    val x =
        return a() // Line breaks are prohibited after return.

    val x =
        return // Line breaks are prohibited after return.
    a() // So this line should not be indented.

    val x =
        return@A a() // Line breaks are prohibited after return.

    val x =
        return@A // Line breaks are prohibited after return.
    a() // So this line should not be indented.

    val x =
        continue

    val x =
        continue@A

    val x =
        break

    val x =
        break@A

    // callableReference
    val x =
        Foo
        ::
        foo

    val x =
        Foo
        ::
        class

    // typeModifier
    val f:
        suspend
        () -> Unit
        =
        suspend
        {
            a()
        }
}

class Foo: Base {
    // memberModifier
    public
        override
        fun f() {
    }

    public
        lateinit
        var x: Int


    // visibilityModifier
    override
        public
        fun f() {
    }

    override
        private
        fun f() {
    }

    override
        internal
        fun f() {
    }

    override
        protected
        fun f() {
    }

    // functionModifier
    public
        tailrec
        infix
        inline
        fun A.f(): A {
        return a()
    }

    public
        operator
        fun A.unaryPlus(): A {
        return a()
    }

    public
        suspend
        fun foo() {
        a()
    }
}

public
    external
    fun foo() {
    a()
}

class Foo {
    // propertyModifier
    public
        const
        val
        x = 1
}

// inheritanceModifier
public
    abstract
    class Foo {
    fun foo() {
        bar()
    }
}

public
    final
    class Foo {
    fun foo() {
        bar()
    }
}

public
    open
    class Foo {
    fun foo() {
    }
}

class Foo {
    // parameterModifier
    fun foo(
        @A
        crossinline
            body: () -> Unit
    ) {
    }

    fun foo(
        @A
        noinline
            body: () -> Unit
    ) {
    }

    fun foo(
        @A
        vararg
            a: A
    ) {
    }

    // reificationModifier
    inline fun <
        @A
        reified
            T // KNOWN_BUG_WHEN_TRUNCATED
    > foo() {
        a()
    }
}

// platformModifier
public
    expect
    class Foo {
    fun foo()
}

public
    actual
    class Foo {
    fun foo()
}


// Ambiguous commas, colons, curly brackets, and objects.

class C: A by object: B1,
                      B2 {
             fun foo() {}
         },
         B by object: B3,
                      B4 {
             fun foo() {}
         } {
    fun <T> foo(x: T): Int {
        return when (x) {
            object: B1 by object: B1 {
                        fun foo() {}
                    },
                    B2 {
                fun foo() {}
            },
            object: B3,
                    B4 {
                fun foo() {}
            } ->
                1

            else ->
                2
        }
    }
}

// Curly braces may appar at:
// classBody
//   class (optional)
//   interface (optional)
//   companionObject (optional)
//   objectDeclaration (optional)
//   enumEntry (optional)
//   objectLiteral
// block
//   init
//   functionBody
//     fun (optional)
//     getter
//     setter
//     anonymousFunction
//   constructor (optional)
//   controlStructureBody (optional)
//     for
//     while
//     do
//     if
//     else
//     whenEntry
//   try
//   catch
//   finally
// lambda
// when

class C:
    A by foo bar { // infix function call
        aaa() // this is not a class body // KNOWN_BUG
    } { // KNOWN_BUG
    // this is the class body

    fun foo() {
        aaa()
    }
}

// Ambiguous arrows

val f = { g:
              (Int) ->
              (Int) ->
              Int ->
    g(1, 2)
}

when (x) {
    1 ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    f as (Int) ->
        Int ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    f(1) ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    (f) ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    is (Int) ->
        Int ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    is Foo<(Int) ->
               Int, // KNOWN_BUG
           (Int) -> // KNOWN_BUG
               Int> -> // KNOWN_BUG
        f as
            (Int) ->
            Int // KNOWN_BUG
    1 < (2), (f) ->
        f as
            (Int) ->
            Int // KNOWN_BUG
    else ->
        f as
            (Int) ->
            Int // KNOWN_BUG
}

// various identifiers

val `abc` =
    1

val `abc def 'ghi "aaa ${ aaa \` = // backquotes cannot be escaped
    a()

// UNICODE_CLASS_ND
val _0123456789٠١٢٣٤٥٦٧٨٩۰۱۲۳۴۵۶۷۸۹߀߁߂߃߄߅߆߇߈߉०१२३४५६७८९০১২৩৪৫৬৭৮৯੦੧੨੩੪੫੬੭੮੯૦૧૨૩૪૫૬૭૮૯୦୧୨୩୪୫୬୭୮୯௦௧௨௩௪௫௬௭௮௯౦౧౨౩౪౫౬౭౮౯೦೧೨೩೪೫೬೭೮೯൦൧൨൩൪൫൬൭൮൯෦෧෨෩෪෫෬෭෮෯๐๑๒๓๔๕๖๗๘๙໐໑໒໓໔໕໖໗໘໙༠༡༢༣༤༥༦༧༨༩၀၁၂၃၄၅၆၇၈၉႐႑႒႓႔႕႖႗႘႙០១២៣៤៥៦៧៨៩᠐᠑᠒᠓᠔᠕᠖᠗᠘᠙᥆᥇᥈᥉᥊᥋᥌᥍᥎᥏᧐᧑᧒᧓᧔᧕᧖᧗᧘᧙᪀᪁᪂᪃᪄᪅᪆᪇᪈᪉᪐᪑᪒᪓᪔᪕᪖᪗᪘᪙᭐᭑᭒᭓᭔᭕᭖᭗᭘᭙᮰᮱᮲᮳᮴᮵᮶᮷᮸᮹᱀᱁᱂᱃᱄᱅᱆᱇᱈᱉᱐᱑᱒᱓᱔᱕᱖᱗᱘᱙꘠꘡꘢꘣꘤꘥꘦꘧꘨꘩꣐꣑꣒꣓꣔꣕꣖꣗꣘꣙꤀꤁꤂꤃꤄꤅꤆꤇꤈꤉꧐꧑꧒꧓꧔꧕꧖꧗꧘꧙꧰꧱꧲꧳꧴꧵꧶꧷꧸꧹꩐꩑꩒꩓꩔꩕꩖꩗꩘꩙꯰꯱꯲꯳꯴꯵꯶꯷꯸꯹０１２３４５６７８９𐒠𐒡𐒢𐒣𐒤𐒥𐒦𐒧𐒨𐒩 =
    1

// Some characters are not supported while they are in the unicode category Nd.
// val _𐴰 =
//     1
// val _𐴱 =
//     1
// val _𐴲 =
//     1
// val _𐴳 =
//     1
// val _𐴴 =
//     1
// val _𐴵 =
//     1
// val _𐴶 =
//     1
// val _𐴷 =
//     1
// val _𐴸 =
//     1
// val _𐴹 =
//     1
val _𑁦 =
    1
val _𑁧 =
    1
val _𑁨 =
    1
val _𑁩 =
    1
val _𑁪 =
    1
val _𑁫 =
    1
val _𑁬 =
    1
val _𑁭 =
    1
val _𑁮 =
    1
val _𑁯 =
    1
val _𑃰 =
    1
val _𑃱 =
    1
val _𑃲 =
    1
val _𑃳 =
    1
val _𑃴 =
    1
val _𑃵 =
    1
val _𑃶 =
    1
val _𑃷 =
    1
val _𑃸 =
    1
val _𑃹 =
    1
val _𑄶 =
    1
val _𑄷 =
    1
val _𑄸 =
    1
val _𑄹 =
    1
val _𑄺 =
    1
val _𑄻 =
    1
val _𑄼 =
    1
val _𑄽 =
    1
val _𑄾 =
    1
val _𑄿 =
    1
val _𑇐 =
    1
val _𑇑 =
    1
val _𑇒 =
    1
val _𑇓 =
    1
val _𑇔 =
    1
val _𑇕 =
    1
val _𑇖 =
    1
val _𑇗 =
    1
val _𑇘 =
    1
val _𑇙 =
    1
val _𑋰 =
    1
val _𑋱 =
    1
val _𑋲 =
    1
val _𑋳 =
    1
val _𑋴 =
    1
val _𑋵 =
    1
val _𑋶 =
    1
val _𑋷 =
    1
val _𑋸 =
    1
val _𑋹 =
    1
val _𑑐 =
    1
val _𑑑 =
    1
val _𑑒 =
    1
val _𑑓 =
    1
val _𑑔 =
    1
val _𑑕 =
    1
val _𑑖 =
    1
val _𑑗 =
    1
val _𑑘 =
    1
val _𑑙 =
    1
val _𑓐 =
    1
val _𑓑 =
    1
val _𑓒 =
    1
val _𑓓 =
    1
val _𑓔 =
    1
val _𑓕 =
    1
val _𑓖 =
    1
val _𑓗 =
    1
val _𑓘 =
    1
val _𑓙 =
    1
val _𑙐 =
    1
val _𑙑 =
    1
val _𑙒 =
    1
val _𑙓 =
    1
val _𑙔 =
    1
val _𑙕 =
    1
val _𑙖 =
    1
val _𑙗 =
    1
val _𑙘 =
    1
val _𑙙 =
    1
val _𑛀 =
    1
val _𑛁 =
    1
val _𑛂 =
    1
val _𑛃 =
    1
val _𑛄 =
    1
val _𑛅 =
    1
val _𑛆 =
    1
val _𑛇 =
    1
val _𑛈 =
    1
val _𑛉 =
    1
val _𑜰 =
    1
val _𑜱 =
    1
val _𑜲 =
    1
val _𑜳 =
    1
val _𑜴 =
    1
val _𑜵 =
    1
val _𑜶 =
    1
val _𑜷 =
    1
val _𑜸 =
    1
val _𑜹 =
    1
val _𑣠 =
    1
val _𑣡 =
    1
val _𑣢 =
    1
val _𑣣 =
    1
val _𑣤 =
    1
val _𑣥 =
    1
val _𑣦 =
    1
val _𑣧 =
    1
val _𑣨 =
    1
val _𑣩 =
    1
val _𑱐 =
    1
val _𑱑 =
    1
val _𑱒 =
    1
val _𑱓 =
    1
val _𑱔 =
    1
val _𑱕 =
    1
val _𑱖 =
    1
val _𑱗 =
    1
val _𑱘 =
    1
val _𑱙 =
    1
// val _𑵐 =
//     1
// val _𑵑 =
//     1
// val _𑵒 =
//     1
// val _𑵓 =
//     1
// val _𑵔 =
//     1
// val _𑵕 =
//     1
// val _𑵖 =
//     1
// val _𑵗 =
//     1
// val _𑵘 =
//     1
// val _𑵙 =
//     1
// val _𑶠 =
//     1
// val _𑶡 =
//     1
// val _𑶢 =
//     1
// val _𑶣 =
//     1
// val _𑶤 =
//     1
// val _𑶥 =
//     1
// val _𑶦 =
//     1
// val _𑶧 =
//     1
// val _𑶨 =
//     1
// val _𑶩 =
//     1
val _𖩠 =
    1
val _𖩡 =
    1
val _𖩢 =
    1
val _𖩣 =
    1
val _𖩤 =
    1
val _𖩥 =
    1
val _𖩦 =
    1
val _𖩧 =
    1
val _𖩨 =
    1
val _𖩩 =
    1
val _𖭐 =
    1
val _𖭑 =
    1
val _𖭒 =
    1
val _𖭓 =
    1
val _𖭔 =
    1
val _𖭕 =
    1
val _𖭖 =
    1
val _𖭗 =
    1
val _𖭘 =
    1
val _𖭙 =
    1
val _𝟎 =
    1
val _𝟏 =
    1
val _𝟐 =
    1
val _𝟑 =
    1
val _𝟒 =
    1
val _𝟓 =
    1
val _𝟔 =
    1
val _𝟕 =
    1
val _𝟖 =
    1
val _𝟗 =
    1
val _𝟘 =
    1
val _𝟙 =
    1
val _𝟚 =
    1
val _𝟛 =
    1
val _𝟜 =
    1
val _𝟝 =
    1
val _𝟞 =
    1
val _𝟟 =
    1
val _𝟠 =
    1
val _𝟡 =
    1
val _𝟢 =
    1
val _𝟣 =
    1
val _𝟤 =
    1
val _𝟥 =
    1
val _𝟦 =
    1
val _𝟧 =
    1
val _𝟨 =
    1
val _𝟩 =
    1
val _𝟪 =
    1
val _𝟫 =
    1
val _𝟬 =
    1
val _𝟭 =
    1
val _𝟮 =
    1
val _𝟯 =
    1
val _𝟰 =
    1
val _𝟱 =
    1
val _𝟲 =
    1
val _𝟳 =
    1
val _𝟴 =
    1
val _𝟵 =
    1
val _𝟶 =
    1
val _𝟷 =
    1
val _𝟸 =
    1
val _𝟹 =
    1
val _𝟺 =
    1
val _𝟻 =
    1
val _𝟼 =
    1
val _𝟽 =
    1
val _𝟾 =
    1
val _𝟿 =
    1
// val _𞅀 =
//     1
// val _𞅁 =
//     1
// val _𞅂 =
//     1
// val _𞅃 =
//     1
// val _𞅄 =
//     1
// val _𞅅 =
//     1
// val _𞅆 =
//     1
// val _𞅇 =
//     1
// val _𞅈 =
//     1
// val _𞅉 =
//     1
// val _𞋰 =
//     1
// val _𞋱 =
//     1
// val _𞋲 =
//     1
// val _𞋳 =
//     1
// val _𞋴 =
//     1
// val _𞋵 =
//     1
// val _𞋶 =
//     1
// val _𞋷 =
//     1
// val _𞋸 =
//     1
// val _𞋹 =
//     1
val _𞥐 =
    1
val _𞥑 =
    1
val _𞥒 =
    1
val _𞥓 =
    1
val _𞥔 =
    1
val _𞥕 =
    1
val _𞥖 =
    1
val _𞥗 =
    1
val _𞥘 =
    1
val _𞥙 =
    1

// UNICODE_CLASS_LL
// UNICODE_CLASS_LM
// UNICODE_CLASS_LO
// UNICODE_CLASS_LT
// UNICODE_CLASS_LU
val aʰªǅA =
    1

// UNICODE_CLASS_NL (rejected for some reason)
// val ᛮ =
//     1
