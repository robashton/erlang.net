using System;
using CsLib;


namespace CsLib.Erlang 
{
  public sealed class Pid : Term
  {
    internal Pid(Runtime runtime, Int64 handle) : base(runtime, handle) {}
  }
}
