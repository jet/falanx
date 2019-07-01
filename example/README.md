# How to use the falanx generator as tool

Falanx generator works as console program

## Install it with dotnet tool

globally

```
dotnet tool install -g Falanx.Tool --version 0.4.*
falanx --help
```

or locally

```
dotnet tool install Falanx.Tool --version 0.4.* --tool-path "tools"
tools/falanx --help
```

## sample1

```
falanx --inputfile ./sample1/bundle.proto --defaultnamespace MyNamespace --outputfile bundle.fs
```
