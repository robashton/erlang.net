Erlang.NET
===

Hosting .NET code in the Erlang VM? Sure can do! 

Getting started
==

*In dotnet*

Create a dotnet library project using .NET 5.0, and add a reference to the package 'Erlang' (Found on Nuget).

```
dotnet add package Erlang --version 1.0.0
```

Define an entry point for your Erlang Application, the simplest one would be a single function returning a single value.

```
  using Erlang;

namespace Acme {
  public class HelloWorld : IApp {
    public Object Start() {
      return "Hello World";
    }
  }
}
```

*In Erlang*

- Add erlang.net to your rebar.config, master will do just fine

```
{deps, [
  {dotnet, {git, "http://github.com/robashton/erlang.net", {branch, "master"}}},
]}.

```
- Add 'dotnet' to the list of applications to start in your app.src

```
  {applications, [
                  kernel,
                  stdlib,
                  dotnet
                 ]},
```

And now, from Erlang, we can invoke that application by writing the following line of code

```
  { ok, Result } = dotnet:run_app_from_assembly("path/to/MyAssembly.dll", "Acme.HelloWorld");
  io:format(user, "Result: ~p~n", [ Result ]);
```


