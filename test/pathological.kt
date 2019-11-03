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
    Bar
    .
    Baz

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
        B, // KNOWN_BUG
        C // KNOWN_BUG
    > // KNOWN_BUG
    = // KNOWN_BUG
    D

// classDeclaration

@A
public
    sealed
    class
    A
    <
        A, // KNOWN_BUG
        B // KNOWN_BUG
    > // KNOWN_BUG
    // primaryConstructor
    public
    constructor // KNOWN_BUG
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
    B, // KNOWN_BUG
    C,
    D
        .
        (Int)
        ->
        Int
    where // KNOWN_BUG
        @A // KNOWN_BUG
        A // KNOWN_BUG
            :
            A,
        B
            :
            B {

    public // KNOWN_BUG
        interface
        A {
       fun a(): Int // KNOWN_BUG
    } // KNOWN_BUG

    // typeParameters
    data
        class
        Bar<
            @A // KNOWN_BUG
            out // KNOWN_BUG
                A
                :
                A,
            @A
            in
                A
                :
                A
        > // KNOWN_BUG
    { // brace on its own line // KNOWN_BUG
        fun a() {
        }
    }

    inner
        enum
        class
        A(val x: Int) {
        // enumClassBody // KNOWN_BUG
        A(1), B(1), // KNOWN_BUG
        C(1) {
            override fun a() = 2
        }, D(1),
        E(1);

        fun a(): Int = x
    } // KNOWN_BUG

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
        fun foo() { // KNOWN_BUG
        }
    } // KNOWN_BUG

    // functionDeclaration
    public
        fun
        <
            A // KNOWN_BUG
        >
        A
        . // KNOWN_BUG
        foo
        // functionValueParameters // KNOWN_BUG
        ( // KNOWN_BUG
            a: Int = 1
        )
        : // KNOWN_BUG
        A
        where
        A : A {
        a() // KNOWN_BUG
    } // KNOWN_BUG

    fun
        foo(x)
        =
        x + 1

    // propertyDeclaration
    public
        val
        <
          A // KNOWN_BUG
        > // KNOWN_BUG
        A
        . // KNOWN_BUG
        @A
        a // KNOWN_BUG
        : // KNOWN_BUG
        A
        where
        A
        :
        A
        =
        a
        get() = 1
        set(value) {
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
        @A get // KNOWN_BUG
        @A set

    // objectDeclaration // KNOWN_BUG
    public // KNOWN_BUG
        object
        Foo
        :
        Bar {
        fun foo() { // KNOWN_BUG
        }
    } // KNOWN_BUG

    // secondaryConstructor
    public
        constructor
        (
        )
        :
        this
        ( // KNOWN_BUG
            1
        ) {
        a() // KNOWN_BUG
    } // KNOWN_BUG

    public // KNOWN_BUG
        constructor
        (
        )
        :
        super
        ( // KNOWN_BUG
            1
        )

    // dynamic type // KNOWN_BUG
    var x: dynamic = 1 // KNOWN_BUG

    // nullableType
    var
        x
        :
        A
        <
            X, // KNOWN_BUG
            *, // KNOWN_BUG
            out
                Y,
            in
                Z
        > // KNOWN_BUG
        . // KNOWN_BUG
        B
        .
        C
        <
            X // KNOWN_BUG
        >
        ? // KNOWN_BUG
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
} // KNOWN_BUG

// statements // KNOWN_BUG
fun foo() { // KNOWN_BUG
    //explicit semicolons
    a();
    b();
    c();

    // annotation
    @A
    // label
    aaa@
    a() // KNOWN_BUG

    // forStatement // KNOWN_BUG
    for ( // KNOWN_BUG
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
        a() // KNOWN_BUG

    // whileStatement // KNOWN_BUG
    while ( // KNOWN_BUG
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
        a() // KNOWN_BUG

    while ( // KNOWN_BUG
        a()
    )
        ; // KNOWN_BUG

    // doWhileStatement // KNOWN_BUG
    do { // KNOWN_BUG
        a()
    } while (
        a()
    )

    do
    { // KNOWN_BUG
        a()
    }
    while // KNOWN_BUG
        (
            a()
        )

    do
        a()
    while (a())

    do
    while (a())

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
        b // KNOWN_BUG

    val x = // KNOWN_BUG
        a <=
        b

    val x =
        a >=
        b

    val x =
        a in // Line breaks are prohibited before in/is operators
        b

    val x =
        a
        !in // KNOWN_BUG
        b // KNOWN_BUG

    when (a()) {
        1 -> a()
        // Line breaks are prohibited before in/is operators, So the following
        // line should not be indented.
        in aaa -> a()
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
    }

    val x =
        a
        ?:
        b

    // infixFunctionCall
    val x =
        a shl // Line breaks are allowed after infix function.
        b // KNOWN_BUG

    // This is not a infix function call; line breaks are // KNOWN_BUG
    // prohibited before infix function. // KNOWN_BUG
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
        a@ // label // KNOWN_BUG
        + // KNOWN_BUG
        -
        ++
        a

    val x =
        --
        a

    val x =
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

    val x =
        a!!

    val x =
        f< // Line breaks are prohibited before type arguments.
            A // KNOWN_BUG
        >( // Line breaks are prohibited before function arguments.
            x
        )[ // Line breaks are prohibited before subscript.
            x
        ]
        . // KNOWN_BUG
        a
        .
        b

    // lambda arguments
    val x = f()
    @A
    a@
    { // KNOWN_BUG
        a()
    }

    val x = f() @A a@ { // KNOWN_BUG
        a()
    }

    val x = f
    @A
    a@
    { // KNOWN_BUG
        a()
    }

    val x = f @A a@ { // KNOWN_BUG
        a()
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
           4, 5, 6,
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
        foo("abc") // KNOWN_BUG
    }ghi${ // KNOWN_BUG
        "aaa\${ // dollar sign cannot be escaped // KNOWN_BUG
            1 // KNOWN_BUG
        }bbb" // KNOWN_BUG
    }jkl" // KNOWN_BUG

    val x = """a
               "b" // KNOWN_BUG
               c${
        foo("""a // KNOWN_BUG
                b // KNOWN_BUG
                  c // KNOWN_BUG
            """) // KNOWN_BUG
    }d // KNOWN_BUG
    e // KNOWN_BUG
    f${
        """aaa\${ // KNOWN_BUG
            1 // KNOWN_BUG
        }bbb""" // KNOWN_BUG
    }ghi""" // KNOWN_BUG

    val x =
        a("'")

    // lambdaLiteral
    val f: () -> Unit = {
        a()
        a()
        a()
    }

    val f: () -> Unit = { ->
        a() // KNOWN_BUG
        a() // KNOWN_BUG
        a()
    }

    val f: () -> Unit = {
        -> // KNOWN_BUG
        a()
        a() // KNOWN_BUG
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
        a() // KNOWN_BUG
        a() // KNOWN_BUG
        a()
    }

    val f: () -> Unit = {
        x,
        y,
        z
        -> // KNOWN_BUG
        a()
        a() // KNOWN_BUG
        a()
    }

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
        B // KNOWN_BUG
        : // KNOWN_BUG
        B {
        a() // KNOWN_BUG
    } // KNOWN_BUG

    // functionLiteral
    val f = fun
    {  // KNOWN_BUG
        a()
    }

    // objectLiteral // KNOWN_BUG
    val x = // KNOWN_BUG
        object
        :
        A,
        B // KNOWN_BUG
            by
            b,
        C {
        fun foo() { // KNOWN_BUG
        }
    } // KNOWN_BUG

    val x = object // KNOWN_BUG
    { // KNOWN_BUG
        fun foo() {
        }
    }

    // thisExpression // KNOWN_BUG
    val x = // KNOWN_BUG
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
            Int // KNOWN_BUG
        >@Foo

     // ifExpression // KNOWN_BUG
     val x = if ( // KNOWN_BUG
         a
     ) {
         a()
     }

     val x = if
         (
             a
         )
     {
         a()
     }

     val x = if (
         a
     )
         a() // KNOWN_BUG

     val x = if ( // KNOWN_BUG
         a
     )
         ; // KNOWN_BUG

     val x = if ( // KNOWN_BUG
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
     { // KNOWN_BUG
         a()
     }

     val x = if ( // KNOWN_BUG
         a
     )
         a() // KNOWN_BUG
     else // KNOWN_BUG
         a()

     val x = if (
         a
     )
         ; // KNOWN_BUG
     else // KNOWN_BUG
         ;

     val x = if (
         a
     )
     else
         ;

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
         { // KNOWN_BUG
             a()
         }

         in // KNOWN_BUG
             a()
             ->
         { // KNOWN_BUG
         }

         is // KNOWN_BUG
             A
             ->
         { // KNOWN_BUG
         }

         a() // KNOWN_BUG
             ->
             a()

         else
             ->
         { // KNOWN_BUG
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

     val x = try
     { // KNOWN_BUG
         a()
     }
     catch // KNOWN_BUG
         (
             @A e: E
         )
     {
         a()
     }
     finally
     { // KNOWN_BUG
         a()
     }

     val x = try { // KNOWN_BUG
         a()
     } finally {
         a()
     }

     val x = try
     { // KNOWN_BUG
         a()
     }
     finally // KNOWN_BUG
     { // KNOWN_BUG
         a()
     }

     // jumpExpression // KNOWN_BUG
     val x = // KNOWN_BUG
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

     // typeModifier // KNOWN_BUG
     val f:
         suspend
         () -> Unit
         =
         suspend
     { // KNOWN_BUG
         a()
     }
}

class Foo: Base {
    // memberModifier
    public
        override
        fun f() {
    } // KNOWN_BUG

    public
        lateinit
        var x: Int


    // visibilityModifier
    override
        public
        fun f() {
    } // KNOWN_BUG

    override
        private
        fun f() {
    } // KNOWN_BUG

    override
        internal
        fun f() {
    } // KNOWN_BUG

    override
        protected
        fun f() {
    } // KNOWN_BUG

    // functionModifier
    public
        tailrec
        infix
        inline
        fun A.f(): A {
        return a() // KNOWN_BUG
    } // KNOWN_BUG

    public
        operator
        fun A.unaryPlus(): A {
        return a() // KNOWN_BUG
    } // KNOWN_BUG

    public
        suspend
        fun foo() {
        a() // KNOWN_BUG
    } // KNOWN_BUG
}

public
    external
    fun foo() {
    a() // KNOWN_BUG
} // KNOWN_BUG

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
    } // KNOWN_BUG
} // KNOWN_BUG

public
    final
    class Foo {
    fun foo() { // KNOWN_BUG
    }
} // KNOWN_BUG

public
    open
    class Foo {
    fun foo() { // KNOWN_BUG
    }
} // KNOWN_BUG

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
        reified // KNOWN_BUG
            T
    > foo() { // KNOWN_BUG
        a()
    }
}

// platformModifier
public
    expect
    class Foo {
    fun foo() // KNOWN_BUG
} // KNOWN_BUG

public
    actual
    class Foo {
    fun foo() // KNOWN_BUG
} // KNOWN_BUG

// various identifiers

val `abc def 'ghi "aaa ${ aaa \` = // backquotes cannot be escaped
    a() // KNOWN_BUG

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
