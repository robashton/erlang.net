using System;
using Erlang;

namespace TestImpl.Tests
{
    public record AddingAppArgs {
      public int X { get; init; }
      public int Y { get; init; }
    }

    public class AddingApp : IApp<AddingAppArgs> {
      public Object Start(AddingAppArgs args)
      {
        return args.X + args.Y;
      }
    }
}
