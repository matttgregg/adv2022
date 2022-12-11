# Advent of Code 2022 

## Intro

It's that time of year again! Twenty-five daily puzzles at [advent of code](adventofcode.com).

This year, I was undecided until the very last minute on what language to write in. The options were:

* Rust - I love Rust. I've used Rust for the past two years though.
* Elixir - I've dipped into on and off for a while. It would be good to work on it properly. My only worry is whether daily puzzles make it easy to play with the most interesting parts of the system: concurrency, networking, and so on.
* Ocaml - Tempting.
* F# - Tempting, and maybe slightly more practical. (We use .NET at work.)
* Julia - I've read zero about it, but know it has it's admirers.
* Common Lisp - I've always enjoyed the lisps, using Racket and Clojure for multiple tools. A friend used it for AoC last year, and his verdict: there are problems with fewer modern libraries, but the interactive development experience is unmatched.
* Haskell - Always a tempting option. Get my head around the system in a practical way.

Towards the end of November I'd almost fixed on Common Lisp. I'd installed an environment (surprisingly easy) and working through some texts - and it was all very enjoyable. I have to say that the development experience is superb: experiment, re-evaluate, re-write, test, all with a few intuitive key presses.

**Then** I got distracted by a tweet from _Rebecca Skinner_, the author of [Effective Haskell](https://www.pragprog.com/titles/rshaskell/effective-haskell/) about the book progressing. I remembered I had ordered the Beta version a while ago and had it on my iPad. I tweeted something along the lines of "Great book, this might be the push I need to do Advent of Code in Haskell this year!"

And after I'd tweeted it, I sort of had to do it, didn't I?

## Day 1

I realised that I hadn't touched Haskell in about five years. I barely remembered how modules worked, and had no idea how to set up a project.

The puzzle was not the issue today. Read numbers delimited by empty lines, find the biggest (three) groups. Not so hard.

My challenges were:

 * How to set up a project in Haskell?
 * How to do IO in Haskell?

I _think_ **ghcup** was new since I'd last tried Haskell. It made some things easier, but it was a bit of a pain making it work with the Visual Code Haskell extension. Mainly involving making some soft links so that the right executables could be found. And ignoring the messages about an incompatible language server. I _want_ a language server, but guess I can use compiler errors for the moment. I definitely needed syntax highlighting though. I gave up on _ormolou_ and ended up with one powered by _stylish-haskell_ which I have no complaints with. 

So, I finally had a working (ish) Haskell environment, a project structure and could write the code for the day. And, to be honest, being able to start a repl loaded with the project modules was nice.

## Day 2

I really liked this one. It wasn't too complicated but like so many just a question of coding all the rules clearly.

On my Haskell adventures I got some practive creating new types for the game state, functions with multiple parts (no if statements today). Slowly feeling a bit more comfortable with what's going on.

## Day 3

Today mapped really neatly into Haskell _with the right library functions_. I think one of the marks of software experience is having a feeling for the sort of functions which probably exist in the standard library. It helps knowing when to look things up versus rolling your own.

So today, I spent more time diving into how Haskell manages imports, and the various functions available for doing set operations on lists.

## Day 4

Well *this* was an exciting day.

Finally decided to look at the parsing challenge. First up, thought I'd try `peggy` as I've used PEG parsers in other language with success. There's also nice documentation. **But** I then find it hasn't been touched for many years, and is incompatible with the version of Haskell that I'm using.

Haskell has a famous parser called `parsec`, and quite influential. I've used `fparsec` in `F#` and so it seems reasonable...

What slowed me down:

 * It's not just `parsec` anymore. `parsec` is the grand-daddy, but now there's `attoparsec` and `magaparsec` and probably more.
 * The documentation for `megaparsec` is dense. There are some good chunky ones there, but often start with describing the types of the core functions. I'm still a Haskell noob, and it's a bit like trying to read a novel in a language that you're still learning.

So this is probably the day that I spend the most in typing things, reading compiler messages, and twiddling them around again. I finally get something working, although I'm not exactly comfortable with the parser library yet.

It's also a day where I massively overcomplicate to start with - instead of comparing in pairs on each line, I compare elves against *all* other elves. And it takes me a while to spot that mistake.

Definitely a learning day.


## Day 5

Today I learn about using a mutable array in Haskell.

It seemed the right way to do things, and it's something that I know anyone serious about the language needs to understand.

So, I spend a good chunk of time reading about `STArray` and friends. Besides *Effective Haskell*, I'm also getting a lot of use out of [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell) by Will Kurt, which is an excellent text. For some reason I'd discounted it as a 'beginners' text, but it gives clear and detailed examples for a lot of good topics. The final lesson is on 'Efficient, stateful arrays in Haskell' - perfect!

Putting it together and writing something doing significant processing within a mutable array is quite rewarding.

## Day 6

A very neat, very fun day. I can drop back to using almost only the core language, and the solution is short and sweet.

One interesting bit was working out how to do an 'n-things different' function. (I didn't want to do a hardcoded one for the lengths requested.)

## Day 7

Writing the processing logic for a text stream is starting to come together now.

It felt quite pleasant writing something with a `State` monad to build up the file structure from the input script. And I learnt how to use the `State` monad.

My processing logic felt nicely organised, and fairly clean for a change.

I did maybe spend too much time on optimizing. Although this is a puzzle open to memoization, in practice I didn't find it was needed.

I got hit by one silly error - I'll only give my thanks to someone who posted a test case on reddit which helped me catch it. 

And having caught it, I reflect that some of my text processing should be cleaner.

As an aside, I made the jump after Day 3 or so to `Text` instead of `String` for most things. And as part of that, the world of language extensions - and overall it hasn't been as painful as I had feared.


## Day 8

Very nice, very mathsy today. I don't think I actually learnt much Haskell, but there was a lot of knowing how to traverse lists as rows of an array. If we come to this again, I might use an array rather than lists of lists - it feels better.

On the other hand, despite relying on a lot of list operations today, my solution was still fast. This meant tracing over all trees in an array, and for each of those looking up the trees up, down, left, right and scanning those. Despite all this however, my solutions were all lightening fast. Either I'd severely overestimated how much processing was involved, or `ghc` is handling the list operations very smartly. At the very least, laziness is proabably lending a hand here too.

I still haven't writtent any tests, and I usually like to at least validate against the test data. Now that I'm more comfortable with the core language, it feels like an achievable aim for the weekend.

## Day 9

I feel like I'm finally getting into the swing of things here. The initial implementation
went in quite neatly - except that my code locked solid when I ran it on actual data.

Putting some debug in, I could see that I was processing every line, but then getting stuck
when trying to export the result. I realised this was probably laziness biting me - I was
building up quite a big lazy list, and then asking it to collapse at the end. I initially, vainly,
tried to fix by using `Text.IO` (i.e. non-lazy) but predictably this had no effect.
Looking a bit deeper, it looked like a a prime bottle neck would be me calling `nub` on every move - this 
is me trying to avoid duplicates in my 'visited locations' list. But, this is running on a list, so
cost O(n) in the length of the list, and as that grows to a few thousand... A quick fix was
changing to just doing one `nub` call at the end. This gave the right answer and was fast enough. 
Then I shifted to using a `Map` rather than a list - and back to pretty instantaneous operations.

The second part - I was initially worried/prepared for a bit of a refactoring slog as it 
required a significantly different architecture pattern. *However* it turned out to be a
relatively straight forward refactor and I accomplished it first time without errors. This
was an area I felt the Haskell type system helping me. As I refactored, the type system 
helped guide the right way to rewrite the functions. Very nice, well done Haskell.

