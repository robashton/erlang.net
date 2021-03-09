using System;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class ProcessResult : Term
  {
    internal ProcessResult(Runtime runtime, ErlNifTerm native) : base(runtime, native) {}
  }
}
