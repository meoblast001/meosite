---
title: Changing the Way I Write Software
---

Every day we all have new experiences. When we practice a profession or hobby,
the new things we learn can affect the way we continue to do old things. As a
software developer, when I'm exposed to new programming languages, libraries,
and tools, the way I use those, which I already know, changes. Until about three
years ago, I was only familiar with imperative programming. In almost every line
of code, I was giving the machine an instruction about **what to do**.

Three years ago I started working with [Ruby](http://www.ruby-lang.org). Of
course Ruby is still an imperative programming language, but it has a lot of
features that promote and facilitate functional programming. When I started
using Ruby I used almost none of them.

Then a half year later I started learning [Clojure](http://clojure.org). Clojure
promotes quite a bit of functional and declarative programming. That is:
programming which respects the mathematical definition of a function and
declares what data is instead of specifying instructions to generate it. In
Clojure you still have parts of your code where you do a lot of imperative
operations with possible side effects (this term will be explained in this
post), but generally Clojure is designed to promote declarative programming, and
the code I wrote in Clojure at the time was more declarative and functional than
any code I had written until that time.

Around the time I started learning Clojure was the same time I started doing
professional Ruby work. The result was interesting. After experiencing Clojure,
the amount of declarative code I wrote increased significantly. Of course there
were still some side effects in there (mostly the database), so you can't say I
was writing functional code, but I was starting to learn how to really use
[higher order functions](/blog/introduction_higher_order_functions/) to explain
**what** my data is instead of **how** it should be computed.

About a year and a half ago I went a step further: I added a strong type system
to my code and eliminated side effects. That is, I learned and began to use
[Haskell](https://www.haskell.org). Some people think Haskell is an extremely
complicated and theoretical language which has no practical application, but
complicated is subjective. If you can get through a mathematics course and an
entire computer science degree, there's no reason you can't understand how to
write Haskell code. That it has no practical application is completely wrong,
and Haskell's practical application can be seen by the vast amount of libraries
that exist for the language and by the fact that it is used commercially by
companies such as Facebook, Google, Intel, NVIDIA, Siemens, and Microsoft.
Therefore I've naturally found quite a few practical applications of the
programming language.

In addition, these programming languages have taught me how to write code
differently in other programming languages... almost all of them. From time to
time (and in my work) I use programming languages that do not enforce or
encourage functional purity, or those with weak type systems. Despite this
drawback, the way I write software in those programming languages has changed a
lot. I would like to reflect on these changes and share them.

### Write Declarative Code

Many programming languages allow you to change data behind a name. For example,
you can set `foobar = 0` and then in the next line change the value of foobar,
possibly using its existing value in this calculation. This is called
mutability. Mutability can of course be somewhat intuitive to the way we think
about some problems: particularly if we think about graph theory.

It turns out uncontrolled mutability can be dangerous though. You could, for
example, have a segment of code that calculates some value and stores it in some
variable and another segment of code that uses this value to come to a
conclusion. Later, another developer finds a reason to insert some code between
both of these segments. In this code, the variable is modified. If attention to
detail isn't paid, the conclusion could be incorrectly determined. This is
usually very avoidable if you're dealing with one procedure's scope (although it
still happens a lot...), but when you're dealing with object/class scope, things
could become messy.

Immutability interestingly doesn't completely destroy the ability to change
values (as weird as this sounds). One possibility is to declare your changed
data under a different symbol name and use this. Any references to the original
data will still refer to exactly that data. You can even take it a step further
and use structures such as state monads in conjunction with syntactical features
of some programming languages to simplify controlled mutability.

Of course I'm not implementing state monads in C++, Java, or Ruby, but I do
pay close attention to mutability and state I introduce into my code and try to
keep it **controllable**. By not allowing the mutability and state that I
introduce to get too complex, and by documenting scenarios that do become
complex (so that they are not easily passed over by other developers), the code
becomes more durable to future changes. Here's a small (possibly trivial)
example in Java:

    public class MutabilityAndImmutability {
      public String withUnnecessaryMutability() {
        String result = "Result is: ";
        // Start adding some code here and modify result however you want.
        if (someCalculation()) {
          result += "Good";
        } else {
          result += "Bad";
        }
        return result;
      }

      public String withoutMutability() {
        final String constOutput = "Result is: ";
        if (someCalculation()) {
          return constOutput + "Good";
        } else {
          return constOutput + "Bad";
        }
      }

      // someCalculation defined somewhere here.
    }

It might seem trivial, but you must directly modify a line involved in
declaring data in the second example, while you can simply add new lines of
code in the first example and make a valid change which may break the result.
Imagine what this could become in a non-trivial example.

### Null Is Rubbish

The [null object pattern](https://en.wikipedia.org/wiki/Null_Object_pattern) has
become very popular in programming languages over the past few decades. It
sometimes seems hard to find programming languages without this concept of
`null`, and therefore it's understandable why many software developers can't
imagine a world without it. It shouldn't be a surprise to anyone that `null`
is dangerous, especially considering the amount of times most of us have seen
`NullPointerException`, `NoMethodError` due to `nil:NilClass`, call to member
function on a non-object, and segmentation faults, sadly even in production
code.

Is all of this really necessary? This concept of `null` is usually used to
represent the lack of something. Something is missing, or an error occurred
which made it impossible to generate the data we want. Of course we need a way
to represent nothingness, but `null` is **NOT** the solution.

<div style="text-align: right;">
  "I call it my billion-dollar mistake. It was the invention of the null
  reference in 1965. [...]  This has led to innumerable errors, vulnerabilities,
  and system crashes, which have probably caused a billion dollars of pain and
  damage in the last forty years."<br />
  \- Tony Hoare, inventor of the null object pattern
</div>

The idea here is not to get rid of nothingness but to restrict what things can
be "nothing". One common solution is the
[option type](https://en.wikipedia.org/wiki/Option_type). It's a very simple
solution. Imagine first of all that `null` is gone. An option type contains
either something of some type, or it contains nothing. The developer can then
query whether there actually is something or not, and then use it. You also can
build a range of convenience functions to work on your option type. It is
important though that you cannot treat your option type the same as you would
treat the content inside. You **must** consider the case in which there is
nothing. Things which are not contained in an option type cannot be nothing
(they must **always** contain a value).

Consider the example of a contact. Within this contact type you require a phone
number. Optionally you can have a fax number. You would therefore say that a
contact (`Contact`) has a phone number entry (`PhoneNumber`) and an option
of a fax number (`Option<FaxNumber>`). The beauty here is that if you have a
`Contact` you definitely have a `PhoneNumber`, no questions asked. And although
you may not have a `FaxNumber`, code using a `Contact` cannot assume that a
`FaxNumber` is present. It must first query the `Option` for its `FaxNumber`,
and in doing so, the case in which there is none is handled. We explicitly
defined what things are required and what things can be nothingness. This is
much safer than if everything could be `null` or not, leaving it up to
interpretation to decide what things we should check for `null` and what things
we shouldn't.

### Consider Your Complete Domain and Codomain

In mathematics a function is nothing more than a map. You have a domain, or the
set of all things which can be passed to your function, and a codomain, the set
of all things that can come out of it. A function of type
`String -> Bool -> Int`, or `int f(string x, bool y)`, has a domain containing
all possible strings which can be formed with all possible boolean values
which can be formed (obviously simply `true` and `false`) and a codomain of
every integer from -&#8734; to &#8734;. An important word associated with
functions is totality. A total function can produce a value in its codomain for
every value in its domain. This means if we can't produce an integer for every
combination of strings and booleans, we have written a partial function. Partial
functions are dangerous.

We have a lot of partial functions that software developers frequently use,
often without noticing. Think of the function `std::vector::front()` in C++. For
`std::vector<T>` it should return a reference to a `T`. Can you think of a case
where this `T` cannot be produced? If you guessed the empty vector, your answer
is right. Or as the code I just compiled to test this example says during
runtime: `Segmentation fault (core dumped)`. Imagine how much fun that would be
in production code! The solution here is for front to return
`std::experimental::optional<T>`, a
[proposed extension to the C++ language](http://en.cppreference.com/w/cpp/utility/optional)
(the `null` problem is a difficult one to avoid; it even pops up in this
section).

### Handle Side Effects With Care

In the functional programming world, people talk a lot about "launching the
missiles". There are even
[jokes about this in package repositories](https://hackage.haskell.org/package/acme-missiles-0.1.0.0/docs/Acme-Missiles.html).
This expression is usually used to describe how dangerous code that interacts
with the outside world can be. To be clear, interacting with the outside world
is **not** a side effect, but it can (and often is) implemented as one. A side
effect is anything that breaks functional purity. Functional purity is the idea
that a function is nothing more than its mathematical definition: a map from a
domain (its input) to a codomain (its return value). The PHP function
`file_get_contents` is definitely not a pure function. Reading that file is
implemented as a side effect. I can call `$a = file_get_contents('test.txt');`,
replace the file, call `$b = file_get_contents('test.txt');` again, and
`$a === $b` will not be true. This is not a valid map. One key goes to two
values.

                ----> "contents that were in the file"
              /
    "test.txt"
              \
                ----> "contents that are now in the file"

One reason we tend to implement I/O as a side effect is because at first it
seems like it's the only way. Results from the outside world are not something
about which we can make many statements at compile time. We can implement I/O
without side effects though, and a great example is
[the IO monad](https://www.haskell.org/tutorial/io.html). I will not go into
length explaining it, but simplified: you pass an action and a handler function
(describing what will be done with the result of this action) to a bind function
and get a representation of this action paired with this handler. Passing the
same action and the same handler function to bind will always map to the same
pair.

This isn't particularly easy to put into practice in programming languages
which are not built with monads in mind (trust me, the syntax gets very ugly
very fast, although it is possible). There is something we can take away from
the IO monad and use in other programming languages though. The wonderful thing
about the IO monad is that the fact that I/O is happening is represented in the
return type. A function that does not have IO in its return type cannot perform
I/O or call any other function which does so. This prevents your
`int addTwoNumbers(int x, int y)` from suddenly deleting System32 or launching
Russia's nuclear arsenal (I'm not claiming you or I have such a procedure which
can do either of those things). This is because it doesn't have IO in its return
type.

If we're using side effects instead of something like an IO monad though, we
can't really define this in the return type. Therefore we need to fall back to
plan B. When the type system doesn't let us specify something, we document it!
In general what this taught me is that if it is not obvious that a procedure in
some language will perform I/O (or sometimes even if it is obvious), I should
document the procedure with information about the interaction it is making with
the outside world.

### Conclusion

I'm probably missing a few things I've learned here, but that's not a problem.
My goal is to share what I've learned and how I've changed the way I write
software. I encourage others (such as you, the reader) to learn a new
programming language with a different programming paradigm from that with which
you are familiar. It will probably do the same for you that it did for me. You
will find the advantages of this new language or paradigm and try to integrate
it into your existing work.
