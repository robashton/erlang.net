using System;
using System.Text.RegularExpressions;
using System.Dynamic;
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Text;
using System.Reflection;
using System.Linq;
using CsLib;

namespace CsLib.Erlang
{
  public sealed class Functions : DynamicObject
  {
    private Runtime runtime;
    private String moduleName;

    public Functions(Runtime runtime, String moduleName) {
      this.runtime = runtime;
      this.moduleName = moduleName;
    }
    
    public override bool TryInvokeMember(InvokeMemberBinder binder, object[] args, out object result) {
      ErlNifTerm term = runtime.CallErlangFn(DotNetToErlang(moduleName), DotNetToErlang(binder.Name), args.Select(x => runtime.ExportAuto(x)).ToArray());
      result = this.runtime.ExtractAuto(term);
      if(result != null) { return true; }
      return false;
    }

    private static string DotNetToErlang(String str) {
      Regex pattern = new Regex(@"[A-Z][a-z]+");
      var matches = pattern.Matches(str);
      return string.Join("_", matches).ToLower();
    }
  }

}
