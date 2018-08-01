# forest-lang

Forest is a functional programming language that compiles to WebAssembly. This repository contains the compiler and core syntaxes, currently implemented in Haskell.

Forest is pre-alpha experimental conceptual research software. Imagine this documentation as a preview of what Forest might be.

Design principles
----

* Ease of collaboration outweighs all other priorities.
* For the sake of collaboration, we agree on structure and semantics, and agree to disagree on syntax.
* Forest will be fast enough to make complex games, so normal web apps will be blazing fast.
* Testing aids collaboration, so it should be as painless as possible.
* Since we want to write tests, effect execution and logic should be separate.
* What if everything was a dataflow graph?

Features
-----

 * Statically typed
 * Pattern matching
 * Immutable datastructures (with mutable optimizations for common cases)
 * Ref-counted, incremental cleanup that can be scheduled. No automatic stop the world GC.
 * Multiple syntaxes, users can create and customize syntaxes, and project between.
 * Automatic code formatting
 * Dev virtual filesystem powered by FUSE to project code into desired syntax.
 * Visual editor
 
FAQ
---

**Why are you making this? What's your point of difference from other languages?**

A few reasons. I work on Cycle.js and build apps with it. I wanted to build a visual editor for Cycle.js, but also wanted to be able to edit a textual representation. Rather than retrofitting a complex system to enable that on top of a language with suboptimal semantics, I preferred to start fresh.

I also started working more with Elm, and while I like many aspects of Elm's type system and syntax, I missed being able to build applications as dataflow graphs.

On top of all of that, I have a keen interest in making games on the web, but I am frustrated by the memory model in JavaScript. The hiccups introduced by uncontrollable stop the world garbage collection tear at my soul. I view WebAssembly as an amazing opportunity to eliminate much of the cruft that bloats the web platform.

**What does the syntax look like?**

Since Forest supports multiple syntaxes, it might look very different to different developers.

The first syntax in development is inspired by Haskell and Elm.

For example, here is fibonacci implemented in Forest:

```elm
fib i =
  case i of
    0 -> 1
    1 -> 1
    i -> fib (i - 2) + fib (i - 1)
```

However, this could just as easily be written using another syntax more comparable to JavaScript:

```js
function fib(i) {
  switch (i) {
    case 0: 1;
    case 1: 1;
    case i: fib(i - 2) + fib(i - 1);
  }
}
```

Notice that while the syntax in these examples differs, the underlying semantics are the same (implicit returns, pattern matching).

**If every dev can use different syntax, what do we store in the repo?**

You only need to store a single representation of the syntax in source control, which we'll call the canonical representation. The syntax for this representation would be agreed by the project's collaborators, but is largely unimportant.

**What's the point of having different syntaxes?**

Syntax is polarising. I know people who love Ruby's syntax, and people who hate it. I know people who love Haskell syntax, and people who hate it. The syntax of your language immediately alienates a large swathe of the community.

Beyond languages, the arguments go on. Tabs vs spaces? Whitespace sensitive or curly braces? Semicolons or not?

If everyone can use the syntax they desire, we don't need to have those arguments anymore. How much time does your team spend talking style?

In current languages, new syntax is headline news. Some of the best parts of ES2015 were simply syntactic sugar. If we push syntax to userland, each developer can have their preferred sugar and we can iterate without the need for major version changes to languages.

Additionally, having a variety of different syntaxes might aid beginners in learning the language and contributing to projects. Maybe the maintainers of a project personally like the Haskell syntax with the [sweet unicode greek alphabet generics](https://hackage.haskell.org/package/wai-cors-0.2.5/docs/src/Network.Wai.Middleware.Cors.html#sshow), but perhaps a new contributor is more accustomed to a Python style syntax. Why shouldn't they be able to work together?

**Won't the docs be in a different syntax than what I prefer? What about code snippets?**

A medium term goal is for the Forest compiler to be written in Forest. This means syntaxes will also be written in Forest, which means we can run them in a web browser. So it would be possible to display docs in your preferred syntax, or at least your favourite core syntax. As for code snippets, we could build a code snippet sharing website that projects the snippet to your preferred syntax.

**How do I edit the code in my preferred syntax? Do I need an editor plugin?**

When working on the project, each developer runs `forest dev`, which mounts a virtual filesystem in the local directory using FUSE, called `dev/`.

`dev/` contains all of the source files, projected into the developer's syntax of choice. The developer can read and write these files using their text editor of choice, modifying the canonical representation, with no need to install an editor plugin. Their syntax automatically generates syntax highlighting files for all common editors.

**What about reviewing changes in the command line and web?**

Source control tools such as git can be configured to diff using `forest diff`, which shows the diffs in the developer's preferred syntax.

When reviewing pull requests on the web, developers use WebExtensions to project the changes to their preferred syntax.

**Aren't immutable data structures memory innefficient? Won't that limit your performance with complex games?**

Immutable data structures can have suboptimal characteristics for some classes of high performance applications. This is due to the need to allocate new memory for every change, and in garbage collected languages the need to cleanup unused previous structures.

In Forest, a simple reference counting strategy is used to keep track of allocated memory. When an immutable update is performed, if there is only a single reference to the memory that is being updated, we can simply update the memory in-place. This saves the need to garbage collect the old version that is no longer referenced.

Forest will automatically free any memory when it is no longer referenced. By default, this happens automatically as the code executes. Users can optionally disable this and instead run incremental cleanup for a specified number of milliseconds. In applications trying to maintain a smooth framerate, this allows for fine control over cleanup pauses.

**Why compile directly to WebAssembly? Why not compile to LLVM and get WASM support for free, along with many other platforms?**

There are a few reasons for this. The first is that I'm interested in learning about WebAssembly, and compiling to it is a great way to learn how it works. The second is that Forest aims to squeeze as much performance out of the browser as is reasonably possible. Compiling directly to WebAssembly means we can ensure we produce the smallest reasonable number of instructions to run a program.

Support for compiling to other platforms is planned, as Forest aspires to be a general purpose language. However, the web comes first.

**How close is Forest to being ready for real use?**

Forest is just a sprout right now, it has a long way to go.

Right now, it supports:
 * numbers
 * infix arithmetic
 * function declaration and calls
 * pattern matching
 * let expressions
 * auto code formatting!
 * strings!
 * memory management (kinda)
 * a type system (simple but there)
 
Critical missing features:
  * lists, hashes, booleans
  * if/else
  * ADTs, union types
  * effects

So, very not ready for even simple applications.

Using Forest
---

First, you'll need to make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed. You will also need a ruby interpreter to run the integration tests.

Next, clone this project and run `make`. If you want to make changes and submit a pull request, make a fork and clone that.

```
git clone https://github.com/forest-lang/forest-compiler.git

cd forest-compiler

make
```

`make` will build Forest and copy the binary to your path. It will also run the tests.

Forest has a single binary right now, named `forest`. It has two commands, `forest build <file>`, which produces text-format WebAssembly, and `forest format <file>`, which reprints the given file to stdout.

Currently you'll need to use another tool to translate the outputted Wast to Wasm. One option is [WAVM](https://github.com/AndrewScheidecker/WAVM). In future `forest` will produce Wasm bytecode directly.

Once you have compiled the WAST to WASM, you can either run it with WAVM or through node/the browser.

See [MDN](https://developer.mozilla.org/en-US/docs/WebAssembly/Using_the_JavaScript_API) for more info.

This is a rough experience right now, and is an area that should be better.

**I have a question not answered here?**

Please [open an issue](https://github.com/forest-lang/forest-compiler/issues/new) and ask questions, offer to help, point out bugs or suggest features.
 
