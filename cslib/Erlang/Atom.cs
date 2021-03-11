using System;
using CsLib;

namespace CsLib.Erlang
{
  public class Atom 
  {
    String rep;

    public Atom(String rep)
    {
      this.rep = rep;
    }

    public static implicit operator string(Atom a) => a.rep;
    public static explicit operator Atom(String s) => new Atom(s);

  }
}
