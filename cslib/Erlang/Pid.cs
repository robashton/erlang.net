using System;
using CsLib;


namespace CsLib.Erlang 
{
  public sealed class Pid : Term
  {
    internal Pid(Runtime runtime, int handle) : base(runtime, handle) {}
  }
}
