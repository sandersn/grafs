module SpanTest
open NUnit.Framework
open System.Collections.Generic
open Types
open Basic
open Util

[<TestFixture>]
type public SpanTester () =
  [<Test>]
  member this.MinSpanKruskalTest () =
    let g = 
      Dictionary.fromList [
        wvertex 'a' [('b',4);('h',8)]
        wvertex 'b' [('a',4);('h',11);('c',8)]
        wvertex 'c' [('b',8);('i',2);('f',4);('d',7)]
        wvertex 'd' [('c',7);('f',14);('e',9)]
        wvertex 'e' [('d',9);('f',10)]
        wvertex 'f' [('g',2);('c',4);('d',14);('e',10)]
        wvertex 'g' [('f',2);('i',6);('h',1)]
        wvertex 'h' [('g',1);('i',7);('a',8)]
        wvertex 'i' [('c',2);('g',6);('h',7)]
      ]
    Assert.That(Span.minKruskal g, Is.EqualTo (set ['a','b'
                                                    'a','h'
                                                    'c','d'
                                                    'c','f'
                                                    'c','i'
                                                    'd','e'
                                                    'f','g'
                                                    'g','h'
                                                   ]))
  [<Test>]
  member this.MinSpanPrimTest () =
    let g = 
      Dictionary.fromList [
        wvertex 'a' [('b',4);('h',8)]
        wvertex 'b' [('a',4);('h',11);('c',8)]
        wvertex 'c' [('b',8);('i',2);('f',4);('d',7)]
        wvertex 'd' [('c',7);('f',14);('e',9)]
        wvertex 'e' [('d',9);('f',10)]
        wvertex 'f' [('g',2);('c',4);('d',14);('e',10)]
        wvertex 'g' [('f',2);('i',6);('h',1)]
        wvertex 'h' [('g',1);('i',7);('a',8)]
        wvertex 'i' [('c',2);('g',6);('h',7)]
      ]
    Assert.That(Span.minPrim g 'a', Is.EqualTo (set ['a','b'
                                                     'b','c'
                                                     'c','d'
                                                     'c','f'
                                                     'c','i'
                                                     'd','e'
                                                     'f','g'
                                                     'g','h'
                                                    ]))