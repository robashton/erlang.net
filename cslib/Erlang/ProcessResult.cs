using System;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class ProcessResult 
  {
    private ErlNifTerm native;

    public ErlNifTerm Native { get { return this.native; }}

    internal ProcessResult(ErlNifTerm native) {
      this.native = native;
    }

  }
}
