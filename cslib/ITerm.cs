using System;
using CsLib.Erlang;

namespace CsLib
{
    public interface ITerm
    {
      ErlNifTerm Native {
        get;
      }

      bool HasValue
      {
        get;
      }
    }
}
