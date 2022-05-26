## Usage

Load modules used by Shadow-CLJS (you only need to do that once):
```shell
npm i
```

Then launch the compiler in watch mode:
```shell
shadow-cljs watch :editor
```

In parallel, launch Girouette's CSS processor in watch mode:
```shell
clojure -X:girouette-processor
```

Browse your webapp by clicking on the link displayed by the compiler
after completion of the compilation.

At that point, the front end Clojurescript code and the CSS will be
automatically reloaded in the browser if you change the source code.
