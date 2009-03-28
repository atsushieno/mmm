
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

		// It is because this rule is not defined in the specification.
		BnfExpression GetNormalCharacterToken ()
		{
			throw new NotImplementedException ();
		}

		public NonTerminal NoneOf (BnfTerm term)
		{
			// FIXME: can we handle them?
			throw new NotImplementedException ();
		}

		public NonTerminal CharacterRange (string start, string end)
		{
			throw new NotImplementedException ();
		}

		// it is required because A to F are regarded as non-terminals in the generator-generator syntax.
		BnfExpression GetHexDigitToken ()
		{
			throw new NotImplementedException ();
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
