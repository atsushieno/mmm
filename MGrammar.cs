
// ===============================================================
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using Irony.Compiler;

namespace MCompiler
{
	public partial class MGrammar : Grammar
	{
		public StringLiteral L (string s)
		{
			return new StringLiteral (s);
		}

		public NonTerminal Opt (BnfTerm elem)
		{
			return new NonTerminal (elem.Name + ".opt") { Rule = Empty + elem };
		}

		internal RegexBasedTerminal Regex (params char [] negativeMatches)
		{
			string p = "[^";
			foreach (char c in negativeMatches)
				p += String.Format ("\\u{0:X04}", (int) c);
			p += "]";
			return new RegexBasedTerminal (p);
		}

		public RegexBasedTerminal CharacterRange (string start, string end)
		{
			return new RegexBasedTerminal ("[" + start + "-" + end + "]");
		}

		// it is required because A to F are regarded as non-terminals in the generator-generator syntax.
		BnfExpression GetHexDigitToken ()
		{
			return L ("0") | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | "A" | "B" | "C" | "D" | "E" | "F" | "a" | "b" | "c" | "d" | "e" | "f";
		}

/*
		BnfExpression GetSignToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDateDaysToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDateMonthsToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetLetterToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetDecimalDigitToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression GetTimeHoursToken ()
		{
			throw new NotImplementedException ();
		}

		BnfExpression StringContent (char sep, bool verbatim)
		{
			throw new NotImplementedException ();
		}
*/

		// some exceptional rules for literals that starts with an uppercase letter.
		StringLiteral T { get { return new StringLiteral ("T"); } }
		StringLiteral Z { get { return new StringLiteral ("Z"); } }
		StringLiteral E { get { return new StringLiteral ("E"); } }
	}
}
