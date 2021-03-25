using System;

namespace Erlang
{
  public class TermException : Exception
  {
    private ErlNifTerm term;

    public ErlNifTerm Term { get { return term; }}

    public TermException(ErlNifTerm term) {
      this.term = term;
    }
    
    public TermException(ErlNifTerm term, string message)
        : base(message) { this.term = term; }

    public TermException(ErlNifTerm term, string message, Exception inner)
        : base(message, inner) { this.term = term; }
  }
}
