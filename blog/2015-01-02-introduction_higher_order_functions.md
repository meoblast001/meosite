---
title: An Introduction to Higher Order Functions
identifier: introduction_higher_order_functions
---

When we create procedures or methods in many programming languages like C, Java,
or C++, we have some inputs and possibly an output. The data that we pass around
are some kinds of values or structures. In C, for example, this may be an
integer, a pointer, a collection of integers and pointers, etc.

To begin talking about higher order functions, we need to start thinking of
functions themselves as things which can be passed around, returned, and
optionally defined literally within other expressions. The definition of a
higher order function is a function which either takes another function as a
parameter, or returns a function (or both).

Many people think that higher order functions are functional programming. This
is incorrect, but the two often go hand in hand. Functional programming is
programming using functions. I'm not referring to procedures though like in
programming languages like C. I'm referring to those algebraic f(x) = y things.
Each function gets parameters, and produces a result based only on those
parameters. Some other related concepts include:

  - Referential transparency and the elimination of side effects (necessary for
    functional purity)
  - Support for higher order functions (unnecessary for functional purity)

### A Brief Look at the Referential Transparency and Side Effects

I don't intend to write much in this post about referential transparency and
side effects, but it's worth taking a moment to briefly explain what they are.

Referential transparency is a property of some expressions which means that if
you know the value of everything making up an expression, you could replace it
with its computed result, and it would make no difference in meaning. For our
"functions" to actually be functions and not just procedures, their expressions
need to be referentially transparent. Because they are truly functions (pure
functions), the only thing they produce is their return value, and the only
variables that can be used to produce this return value are its parameters. But
what if the function needs to do something more than just take input and return
some value? What if it needs to read a file, or (to reuse an overused example)
launch some missiles? In a referentially transparent world, we can only do these
things by representing the actions through the parameters and the return type.
If this were not the case, and we could just open a file or launch the missiles
whenever, we've introduced a side effect, an action which is not represented in
the parameters or return type. This "function" would break the rules of
referential transparency and functional purity and no longer be a function, but
simply a procedure.

To better understand referential transparency, suppose we have the function
f(x) = 5 * x + 10. If we have another function, g(y) = y - f(1), we could
entirely remove the reference to f, and write our function g as g(y) = y - 15.
An example of an impure function would be C's printf. If we tried to replace
printf in an expression in the way we replaced the function "f" above, our
program would act differently. This is because printf has a side effect: putting
text on the console.

Referential transparency may sound scary to someone who is new to functional
programming, but it's not really that bad. I will not be discussing this topic
any further in this blog post, but I invite you to research them on your own. I
promise you won't regret it.

### First Look at Higher Order Functions

As mentioned, higher order functions are simply functions which either take
other functions as parameters or return them in their return types. The
function receiving this function can then call it or pass it along further to
other functions.

Frequently we use higher order functions to help us leave a gap in our code
which the user must describe for us. This code we leave out is often
specialised. By utilising a higher order function, we make our code more
general-purpose. For example, if you create a sort function for a list, you may
want to leave out the part that determines which item goes first. This leaves it
up to the user to determine which rules will be used to sort.

Often we write anonymous functions (frequently referred to as lambdas) which we
pass to other functions. The syntax of an anonymous function is usually more
compact than the syntax for named functions, so that they can be more easily
written inline into your existing expressions.

When you start using higher order functions frequently in your programs, you
often will perform many transformations to data before giving it a name. This is
contrary to more imperative code in which temporary variables are often
constructed which serve as temporary memory used to make your data
transformations. Higher order functions will start to make your code look like
a pipeline of data transformations from one input to an output. We begin to
describe what the data is instead of how you get it.

### "Functional Programming" is Becoming Popular

Programming languages that enforce purely functional code like Haskell are
becoming more popular, while other programming languages that take inspiration
from those programming languages are becoming popular too. Programming languages
such as Clojure and Scala support higher order functions and lambdas. C#'s Linq
library includes a special syntax for lambdas, and Java 8 introduced syntactic
sugar which makes lambdas much easier to write than in previous versions. Ruby
and Python also support higher order functions and lambdas.

### Map / Fold / Filter

We've talked a lot about higher order functions, but we don't have any that we
can actually use yet. Well, why don't we learn about some very common ones?
Let's do that by explaining some functions which are commonly used on
collections of data.

#### Map

Suppose you're writing a program which manages student records at a university.
You have a function which takes a Course data structure and returns a list of
all student names. Here's what this code might look like in C#:

    public string[] GetStudentNames(Course course) {
        string[] student_names = new string[course.students.Count()];
        for (int i = 0; i < course.students.Count(); ++i)
            student_names[i] = course.students[i].first_name + " " + course.students[i].last_name;
        return student_names;
    }

Not only is this code difficult to comprehend at first glance, but there are a
lot of opportunities to make array bounds errors. There has to be a better way
to do this. Well no, there isn't.

Just kidding! Yes there is. We have a higher order function which can help us
here, and it's called "map". Map takes two parameters: a function from some
type (let's call it "a") to another type ("b"), and a list containing items of
type "a". Let's represent the function in the first parameter as (a -> b),
because the function gets type "a" as a parameter and gives back type "b". The
function (a -> b) is describing a transformation to apply to every item in the
list in order to make another list with an equal amount of items of type "b". If
you assume Function<T1, T2> is a function from T1 to T2, a more Java/C#-like
representation might look like this:

    List<T2> map(Function<T1, T2> function, List<T1> input_list)

Map has different names in different programming languages. Here's a listing of
some:

  - Haskell / Clojure / Ruby: map
  - C#: Select

Let's see if we can solve our student record problem with map now:

    public string[] GetStudentNames(Course course) {
        return course.students.Select(student => student.first_name + " " +
                                                 student.last_name).ToArray();
    }

Oh wow, that's clean! Also we have no possibility of bounds errors! Select (name
for map in C#) is simply going to operate over every student record and populate
a list using the transformations we specified. You might be wondering about that
"ToArray" thing though. That's specific to C#. Select returns an Enumerable and
we need to convert that to an array.

Here's how we could solve the same problem in Ruby:

    def get_student_records(course)
      course.students.map { |student| student.first_name + student.last_name }
    end

##### Map All the Functors!

Map is not just a function for lists. I'm going to get a little bit more
abstract here, but you're a programmer! You can handle it! Map operates over all
functors. A functor is anything that can implement map where two laws hold:

  - If the transformation function for map returns its parameter unmodified, the
    functor should be returned unmodified: some_functor.map(x => x) == some_functor
  - Map should compose: some_functor.map(x => f(g(x))) ==
    some_functor.map(x => g(x)).map(x => f(x)).

A functor has to have one function which works over it: map. This is what
our map is when we are working with lists. With a little help from
[a presentation from Tony Morris](https://github.com/tonymorris/comonads-applicative-monads/blob/master/mappable.tex),
here's what map looks like, but keep in mind that it is not valid Java code,
because Java's type system does not allow us to properly define the Functor
interface:

    interface Functor<F> {
        <T1, T2> F<T2> map(Function<T1, T2> function, F<T1> input_functor)
    }

In this signature, we are indicating that F can be anything which is a functor.
We've already seen what the "map" function looks like for list and if you look
back up to its signature, it looks strikingly similar to our "map" signature for
a functor.

Another common functor is "Option", sometimes called "Maybe". It's a data
structure which may contain some data, or it may contain nothing. Option is an
alternative to using "null", but leverages the type system to gain some safety.
An Option<string> either contains "some string" or "nothing". It may not look
like it at first, but this functor has a definition of "map"; it needs to
in order to be a functor. What does it do though? If our Option contains an
inner value, we apply our transformation function to that inner value and return
a new Option containg the result. If it contains nothing, well... we just get an
Option containing nothing back. Let's look at an example using a syntax which
matches C#'s Linq syntax:

    var opt1 = Option<string>.Some("I'm an example");
    var opt2 = Option<string>.Nothing();

    print(opt1.Select(x => x + "!").ToString());
    print(opt2.Select(x => x + "!").ToString());

If we look at the output, we'd get:

    I'm an example!
    [Nothing]

The transformation was only made if there was something to transform.

Well, that's map, let's move on.

#### Filter

Suppose we now only are interested in looking at which students are failing our
course so that we can provide them assistance. How do we solve this
imperatively?

    public Student[] GetFailingStudents(Course course) {
        List<Student> failing_students = new List<Student>();
        foreach (var student in course.students)
            if (student.score < MAX_FAILING_SCORE)
              failing_students.Add(student);
        return failing_students.ToArray();
    }

Here I used a List and foreach syntax in C# to eliminate the possibility of
bounds errors, but this is still a lot of code just to say that I want to only
see failing students. Maybe we have a higher order function that can help.

The filter function also takes one function from some type "a" to a boolean
(a -> Bool) and a list of items of type "a". The first parameter is called a
predicate, which is a mathematical term which means any function which returns
a boolean. Filter applies this predicate to every item in the list, and if it
returns true, the item is added to the output list, otherwise it's dropped.
Here's filter's type signature:

    List<T1> filter(Function<T1, boolean> function, List<T1> input_list)

Filter has different names in different programming languages. Here's a listing
of some:

  - Haskell / Clojure: filter
  - Ruby: select
  - C#: Where

Let's use this to solve our failing students problem now!

    public Student[] GetFailingStudents(Course course) {
        return course.students.Where(student => student.score < MAX_FAILING_SCORE).
                      ToArray();
    }

That was exceptionally easy! Where (name for filter in C#) simply outputs a
collection, which is a duplicate of the original array, removing all students
where the predicate (having a failing score) returns false.

Let's look at the solution in Ruby:

    def get_failing_students(course)
      course.students.select { |student| student.score < MAX_FAILING_SCORE }
    end

#### Fold

Suppose our record system keeps track of how many incomplete assignments each
student has. We now want to see how many assignments are missing in the entire
class.

    public int GetTotalMissingAssignments(Course course) {
        int missing_assignments = 0;
        foreach (var student in course.students)
            missing_assignments += student.missing_assignments;
        return missing_assignments;
    }

This one actually doesn't look too bad, but we can do better. The fold function
has two variations, but they are very similar.

The first fold function takes a function which takes two parameters of type "a"
and returns a value of type "a" (a -> a -> a) and a list of items of type "a".
It starts by taking the first two items and applying our function on them. It
then takes this result and the third item in the list, and runs the function
again, continuously taking the result of the last computation and the next item
in the list, then running the function on them. Finally when it runs out of
items in the list, it returns the result of the last call to the parameter
function. There are two caveats though: if the list only has one item, it just
returns that item; if the list has no items, fold fails.

The second variation is similar, but it takes an additional parameter and cannot
fail. The additional parameter is an initial value of type "b". Our parameter
function also changes its type signature to (b -> a -> b). That is, the type
of our intial value and our list do not need to match, as long as our result
always matches the type of our initial value.

Fold has different names in different programming languages. Here's a listing of
some:

  - Haskell: foldl, foldr (initial value); foldl1, foldr1 (no initial value)
  - Clojure / Ruby: reduce
  - C#: Aggregate

Let's solve our problem using fold now:

    public int GetTotalMissingAssignments(Course course) {
        return course.students.Aggregate(0 /*Initial value.*/, (current_total, student) =>
                current_total + student.missing_assignments
            ).ToArray();
    }

Aggregate (name for fold in C#) made our code a little bit more compact. Of
course, there are much better examples of fold. It may be the most complicated
of the higher order functions described, but sometimes it can be one of the most
useful.

Let's look at the solution in Ruby:

    def get_total_missing_assignments(course)
      course.students.reduce(0) { |current_total, student| current_total + student }
    end

### Conclusion

So now you've learned a little bit about functional programming, higher order
functions, and you've seen some common higher order functions in use (map,
filter, and fold). I hope this introduction can help you start using them in
your own code, and I hope it inspires you to dig deeper into functional
programming.

### Presentation

Update: I created a presentation on this topic which I presented at the
[Youngstown State University chapter of the Association for Computing Machinery](http://ysu.acm.org).
[[Download the Presentation](../../docs/IntroHOF.pdf)]
