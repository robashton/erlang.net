using System;
using CsLib;


namespace CsLib.Erlang
{
  public sealed class Pid : Term
  {
    internal Pid(Runtime runtime, ErlNifTerm native) : base(runtime, native) {}
  }
}
