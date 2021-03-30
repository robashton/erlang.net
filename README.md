Erlang.NET
===

Hosting .NET code in the Erlang VM? Sure can do, but what does this look like?!

Well - here is an Erlang Gen Server written in VB.NET

```vb
    Public Class MyGenServerVB
        Inherits IHandleInfo(Of Msg)
        Public Sub New()
        End Sub
        Public Function HandleInfo(ByVal ctx As HandleInfoContext, ByVal msg As Msg) As HandleInfoResult
          If msg.Item1 = "hello robert" Then
            Erlang.Send(msg.Item2, "hello joe")
          Else
            Erlang.Send(msg.Item2, "weeee")
          End If
          Return ctx.NoReply()
        End Function
    End Class
```

Here is an application that uses this gen server, written in C#


```csharp
  public class MyApp : IApp {
    public Object Start() {
      return GenServer.StartLink(() => new MyGenServerVB() );
    }
  }
```


And here is the usage of that in an Erlang supervision tree - written in Erlang


```erlang
  init([]) ->
    {ok, { #{ strategy => one_for_one }, 
           #{ start => { dotnet_shim
                       , start_link
                       , [ "priv/MyApp.dll", "Acme.MyApp" ]
                       }
            , id => my_app
            , type => worker
            }
       ]}}.
```


And then in Erlang we can quite happily get the pid of this gen server and send/receive messages, resulting in code being executed across the two VMs in both languages.

```erlang

  Pid ! { "hello robert", self() },
  receive 
    Msg -> io:format(user, "Got a message! ~p~n", [ Msg ]) %% "hello joe
  end,

  Pid ! { "anything else", self() },
  receive 
    Msg2 -> io:format(user, "Got another message! ~p~n", [ Msg2 ]) %% "weeeeee
  end

```

Want to know more? Check out the [Wiki](https://github.com/robashton/erlang.net/wiki/Getting-Started) for a step by step guide on how to do more...
