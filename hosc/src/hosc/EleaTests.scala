
package hosc 

import org.junit.Test
import org.junit.Assert._

import Util._

class EleaTests {

  @Test def z2() = testEq("elea/z2-1.hs", "elea/z2-2.hs")
  @Test def z9() = testEq("elea/z9-1.hs", "elea/z9-2.hs")
  @Test def z11() = testEq("elea/z11-1.hs", "elea/z11-2.hs")
  @Test def z12() = testEq("elea/z12-1.hs", "elea/z12-2.hs")
  @Test def z13() = testEq("elea/z13-1.hs", "elea/z13-2.hs")
	@Test def z14() = testEq("elea/z14-1.hs", "elea/z14-2.hs")
  @Test def z19() = testEq("elea/z19-1.hs", "elea/z19-2.hs")
  @Test def z22() = testEq("elea/z22-1.hs", "elea/z22-2.hs")
  @Test def z31() = testEq("elea/z31-1.hs", "elea/z31-2.hs")
  @Test def z35() = testEq("elea/z35-1.hs", "elea/z35-2.hs")
  @Test def z36() = testEq("elea/z36-1.hs", "elea/z36-2.hs")
  @Test def z39() = testEq("elea/z39-1.hs", "elea/z39-2.hs")
  @Test def z40() = testEq("elea/z40-1.hs", "elea/z40-2.hs")
  @Test def z41() = testEq("elea/z41-1.hs", "elea/z41-2.hs")
  @Test def z42() = testEq("elea/z42-1.hs", "elea/z42-2.hs")
  @Test def z45() = testEq("elea/z45-1.hs", "elea/z45-2.hs")
  @Test def z46() = testEq("elea/z46-1.hs", "elea/z46-2.hs")
  @Test def z50() = testEq("elea/z50-1.hs", "elea/z50-2.hs")
  @Test def z55() = testEq("elea/z55-1.hs", "elea/z55-2.hs")
  @Test def z56() = testEq("elea/z56-1.hs", "elea/z56-2.hs")
  @Test def z58() = testEq("elea/z58-1.hs", "elea/z58-2.hs")
  @Test def z61() = testEq("elea/z61-1.hs", "elea/z61-2.hs")
  @Test def z80() = testEq("elea/z80-1.hs", "elea/z80-2.hs")
  @Test def z81() = testEq("elea/z81-1.hs", "elea/z81-2.hs")
  @Test def z82() = testEq("elea/z82-1.hs", "elea/z82-2.hs")
  @Test def z83() = testEq("elea/z83-1.hs", "elea/z83-2.hs")
  @Test def z84() = testEq("elea/z84-1.hs", "elea/z84-2.hs")
  
  @Test def p12() = testEq("elea/p12-1.hs", "elea/p12-2.hs")
  @Test def p22() = testEq("elea/p22-1.hs", "elea/p22-2.hs")
  @Test def p24() = testEq("elea/p24-1.hs", "elea/p24-2.hs")
  @Test def p27() = testEq("elea/p27-1.hs", "elea/p27-2.hs")
  @Test def p28() = testEq("elea/p28-1.hs", "elea/p28-2.hs")
  @Test def p33() = testEq("elea/p33-1.hs", "elea/p33-2.hs")
  @Test def p34() = testEq("elea/p34-1.hs", "elea/p34-2.hs")
  @Test def p35() = testEq("elea/p35-1.hs", "elea/p35-2.hs")
  
  
  def testEq(f1: String, f2: String) = {
    val p1 = supercompile(f1)
    val p2 = supercompile(f2)
    assertTrue(f1 + " and " + f2 + " should be equivalent", Eq.equivalent(p1.goal, p2.goal))
  }
  
	def supercompile(file: String) = {
		val program = programFromFile(file)
		val sc = new SuperCompiler0(program)
		val pt = sc.buildProcessTree(program.goal)
		val g = new CodeConstructor(program, pt, true)
		val p = g.generateProgram()
		p
	}
}