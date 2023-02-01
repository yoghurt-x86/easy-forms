
example () {
    cd examples/$1/
    elm make src/Main.elm --optimize --output="main.js"
    cd ../../

    mkdir -p ./docs/examples/$1
    mv examples/$1/main.js $_
    cp examples/index.html $_
}

example 01
example 02
example 03
example 04
