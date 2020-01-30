// RUN: %clang_cc1 -triple x86_64-unknown-unknown -ast-dump=json -ast-dump-filter Test %s | FileCheck %s

struct S {
  void Test1();
  void Test2() const;
  void Test3() volatile;
  void Test4() &;
  void Test5() &&;
  virtual void Test6(float, int = 12);
  virtual void Test7() = 0;
};

struct T : S { // T is not referenced, but S is
  void Test6(float, int = 100) override;
};

struct U {
  void Test1();
};
void U::Test1() {} // parent

void Test1();
void Test2(void);
void Test3(int a, int b);
void Test4(int a, int b = 12);
constexpr void Test5(void);
static void Test6(void);
extern void Test7(void);
inline void Test8(void);
void Test9(void) noexcept;
void Test10(void) noexcept(false);
void Test11(void) noexcept(1);

template <typename T>
T Test12(T&);

void Test13(int) {}
void Test14(int, ...) {}

int main() {
  Test1(); // Causes this to be marked 'used'
}

// NOTE: CHECK lines have been autogenerated by gen_ast_dump_json_test.py


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 124,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 4,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 119,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 130,
// CHECK-NEXT:    "col": 14,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test1",
// CHECK-NEXT:  "mangledName": "_ZN1S5Test1Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 140,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 5,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 135,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 148,
// CHECK-NEXT:    "col": 16,
// CHECK-NEXT:    "tokLen": 5
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test2",
// CHECK-NEXT:  "mangledName": "_ZNK1S5Test2Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () const"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 162,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 6,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 157,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 170,
// CHECK-NEXT:    "col": 16,
// CHECK-NEXT:    "tokLen": 8
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test3",
// CHECK-NEXT:  "mangledName": "_ZNV1S5Test3Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () volatile"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 187,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 7,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 182,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 195,
// CHECK-NEXT:    "col": 16,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test4",
// CHECK-NEXT:  "mangledName": "_ZNR1S5Test4Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () &"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 205,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 8,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 200,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 213,
// CHECK-NEXT:    "col": 16,
// CHECK-NEXT:    "tokLen": 2
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test5",
// CHECK-NEXT:  "mangledName": "_ZNO1S5Test5Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () &&"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 232,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 9,
// CHECK-NEXT:   "col": 16,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 219,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 7
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 253,
// CHECK-NEXT:    "col": 37,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test6",
// CHECK-NEXT:  "mangledName": "_ZN1S5Test6Efi",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (float, int)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "virtual": true,
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 243,
// CHECK-NEXT:     "col": 27,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 238,
// CHECK-NEXT:      "col": 22,
// CHECK-NEXT:      "tokLen": 5
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 238,
// CHECK-NEXT:      "col": 22,
// CHECK-NEXT:      "tokLen": 5
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "float"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 249,
// CHECK-NEXT:     "col": 33,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 245,
// CHECK-NEXT:      "col": 29,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 251,
// CHECK-NEXT:      "col": 35,
// CHECK-NEXT:      "tokLen": 2
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    },
// CHECK-NEXT:    "init": "c",
// CHECK-NEXT:    "inner": [
// CHECK-NEXT:     {
// CHECK-NEXT:      "id": "0x{{.*}}",
// CHECK-NEXT:      "kind": "IntegerLiteral",
// CHECK-NEXT:      "range": {
// CHECK-NEXT:       "begin": {
// CHECK-NEXT:        "offset": 251,
// CHECK-NEXT:        "col": 35,
// CHECK-NEXT:        "tokLen": 2
// CHECK-NEXT:       },
// CHECK-NEXT:       "end": {
// CHECK-NEXT:        "offset": 251,
// CHECK-NEXT:        "col": 35,
// CHECK-NEXT:        "tokLen": 2
// CHECK-NEXT:       }
// CHECK-NEXT:      },
// CHECK-NEXT:      "type": {
// CHECK-NEXT:       "qualType": "int"
// CHECK-NEXT:      },
// CHECK-NEXT:      "valueCategory": "rvalue",
// CHECK-NEXT:      "value": "12"
// CHECK-NEXT:     }
// CHECK-NEXT:    ]
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 271,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 10,
// CHECK-NEXT:   "col": 16,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 258,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 7
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 281,
// CHECK-NEXT:    "col": 26,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test7",
// CHECK-NEXT:  "mangledName": "_ZN1S5Test7Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "virtual": true,
// CHECK-NEXT:  "pure": true
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 343,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 14,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 338,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 367,
// CHECK-NEXT:    "col": 32,
// CHECK-NEXT:    "tokLen": 8
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test6",
// CHECK-NEXT:  "mangledName": "_ZN1T5Test6Efi",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (float, int)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 354,
// CHECK-NEXT:     "col": 19,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 349,
// CHECK-NEXT:      "col": 14,
// CHECK-NEXT:      "tokLen": 5
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 349,
// CHECK-NEXT:      "col": 14,
// CHECK-NEXT:      "tokLen": 5
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "float"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 360,
// CHECK-NEXT:     "col": 25,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 356,
// CHECK-NEXT:      "col": 21,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 362,
// CHECK-NEXT:      "col": 27,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    },
// CHECK-NEXT:    "init": "c",
// CHECK-NEXT:    "inner": [
// CHECK-NEXT:     {
// CHECK-NEXT:      "id": "0x{{.*}}",
// CHECK-NEXT:      "kind": "IntegerLiteral",
// CHECK-NEXT:      "range": {
// CHECK-NEXT:       "begin": {
// CHECK-NEXT:        "offset": 362,
// CHECK-NEXT:        "col": 27,
// CHECK-NEXT:        "tokLen": 3
// CHECK-NEXT:       },
// CHECK-NEXT:       "end": {
// CHECK-NEXT:        "offset": 362,
// CHECK-NEXT:        "col": 27,
// CHECK-NEXT:        "tokLen": 3
// CHECK-NEXT:       }
// CHECK-NEXT:      },
// CHECK-NEXT:      "type": {
// CHECK-NEXT:       "qualType": "int"
// CHECK-NEXT:      },
// CHECK-NEXT:      "valueCategory": "rvalue",
// CHECK-NEXT:      "value": "100"
// CHECK-NEXT:     }
// CHECK-NEXT:    ]
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "OverrideAttr",
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 367,
// CHECK-NEXT:      "col": 32,
// CHECK-NEXT:      "tokLen": 8
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 367,
// CHECK-NEXT:      "col": 32,
// CHECK-NEXT:      "tokLen": 8
// CHECK-NEXT:     }
// CHECK-NEXT:    }
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 399,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 18,
// CHECK-NEXT:   "col": 8,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 394,
// CHECK-NEXT:    "col": 3,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 405,
// CHECK-NEXT:    "col": 14,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test1",
// CHECK-NEXT:  "mangledName": "_ZN1U5Test1Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "CXXMethodDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 419,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 20,
// CHECK-NEXT:   "col": 9,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 411,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 428,
// CHECK-NEXT:    "col": 18,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "parentDeclContextId": "0x{{.*}}",
// CHECK-NEXT:  "previousDecl": "0x{{.*}}",
// CHECK-NEXT:  "name": "Test1",
// CHECK-NEXT:  "mangledName": "_ZN1U5Test1Ev",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "CompoundStmt",
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 427,
// CHECK-NEXT:      "col": 17,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 428,
// CHECK-NEXT:      "col": 18,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    }
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 446,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 22,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 441,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 452,
// CHECK-NEXT:    "col": 12,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "isUsed": true,
// CHECK-NEXT:  "name": "Test1",
// CHECK-NEXT:  "mangledName": "_Z5Test1v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 460,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 23,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 455,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 470,
// CHECK-NEXT:    "col": 16,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test2",
// CHECK-NEXT:  "mangledName": "_Z5Test2v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 478,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 24,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 473,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 496,
// CHECK-NEXT:    "col": 24,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test3",
// CHECK-NEXT:  "mangledName": "_Z5Test3ii",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (int, int)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 488,
// CHECK-NEXT:     "col": 16,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 484,
// CHECK-NEXT:      "col": 12,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 488,
// CHECK-NEXT:      "col": 16,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "name": "a",
// CHECK-NEXT:    "mangledName": "_ZZ5Test3iiE1a",
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 495,
// CHECK-NEXT:     "col": 23,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 491,
// CHECK-NEXT:      "col": 19,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 495,
// CHECK-NEXT:      "col": 23,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "name": "b",
// CHECK-NEXT:    "mangledName": "_ZZ5Test3iiE1b",
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    }
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 504,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 25,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 499,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 527,
// CHECK-NEXT:    "col": 29,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test4",
// CHECK-NEXT:  "mangledName": "_Z5Test4ii",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (int, int)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 514,
// CHECK-NEXT:     "col": 16,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 510,
// CHECK-NEXT:      "col": 12,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 514,
// CHECK-NEXT:      "col": 16,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "name": "a",
// CHECK-NEXT:    "mangledName": "_ZZ5Test4iiE1a",
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 521,
// CHECK-NEXT:     "col": 23,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 517,
// CHECK-NEXT:      "col": 19,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 525,
// CHECK-NEXT:      "col": 27,
// CHECK-NEXT:      "tokLen": 2
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "name": "b",
// CHECK-NEXT:    "mangledName": "_ZZ5Test4iiE1b",
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    },
// CHECK-NEXT:    "init": "c",
// CHECK-NEXT:    "inner": [
// CHECK-NEXT:     {
// CHECK-NEXT:      "id": "0x{{.*}}",
// CHECK-NEXT:      "kind": "IntegerLiteral",
// CHECK-NEXT:      "range": {
// CHECK-NEXT:       "begin": {
// CHECK-NEXT:        "offset": 525,
// CHECK-NEXT:        "col": 27,
// CHECK-NEXT:        "tokLen": 2
// CHECK-NEXT:       },
// CHECK-NEXT:       "end": {
// CHECK-NEXT:        "offset": 525,
// CHECK-NEXT:        "col": 27,
// CHECK-NEXT:        "tokLen": 2
// CHECK-NEXT:       }
// CHECK-NEXT:      },
// CHECK-NEXT:      "type": {
// CHECK-NEXT:       "qualType": "int"
// CHECK-NEXT:      },
// CHECK-NEXT:      "valueCategory": "rvalue",
// CHECK-NEXT:      "value": "12"
// CHECK-NEXT:     }
// CHECK-NEXT:    ]
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 545,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 26,
// CHECK-NEXT:   "col": 16,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 530,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 9
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 555,
// CHECK-NEXT:    "col": 26,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test5",
// CHECK-NEXT:  "mangledName": "_Z5Test5v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "constexpr": true
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 570,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 27,
// CHECK-NEXT:   "col": 13,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 558,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 6
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 580,
// CHECK-NEXT:    "col": 23,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test6",
// CHECK-NEXT:  "mangledName": "_ZL5Test6v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "storageClass": "static"
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 595,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 28,
// CHECK-NEXT:   "col": 13,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 583,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 6
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 605,
// CHECK-NEXT:    "col": 23,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test7",
// CHECK-NEXT:  "mangledName": "_Z5Test7v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "storageClass": "extern"
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 620,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 29,
// CHECK-NEXT:   "col": 13,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 608,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 6
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 630,
// CHECK-NEXT:    "col": 23,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test8",
// CHECK-NEXT:  "mangledName": "_Z5Test8v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void ()"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inline": true
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 638,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 30,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 5
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 633,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 650,
// CHECK-NEXT:    "col": 18,
// CHECK-NEXT:    "tokLen": 8
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test9",
// CHECK-NEXT:  "mangledName": "_Z5Test9v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () noexcept"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 665,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 31,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 6
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 660,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 692,
// CHECK-NEXT:    "col": 33,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test10",
// CHECK-NEXT:  "mangledName": "_Z6Test10v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () noexcept(false)"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 700,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 32,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 6
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 695,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 723,
// CHECK-NEXT:    "col": 29,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test11",
// CHECK-NEXT:  "mangledName": "_Z6Test11v",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void () noexcept(1)"
// CHECK-NEXT:  }
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionTemplateDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 751,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 35,
// CHECK-NEXT:   "col": 3,
// CHECK-NEXT:   "tokLen": 6
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 727,
// CHECK-NEXT:    "line": 34,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 8
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 760,
// CHECK-NEXT:    "line": 35,
// CHECK-NEXT:    "col": 12,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test12",
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "TemplateTypeParmDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 746,
// CHECK-NEXT:     "line": 34,
// CHECK-NEXT:     "col": 20,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 737,
// CHECK-NEXT:      "col": 11,
// CHECK-NEXT:      "tokLen": 8
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 746,
// CHECK-NEXT:      "col": 20,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "isReferenced": true,
// CHECK-NEXT:    "name": "T",
// CHECK-NEXT:    "tagUsed": "typename",
// CHECK-NEXT:    "depth": 0,
// CHECK-NEXT:    "index": 0
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "FunctionDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 751,
// CHECK-NEXT:     "line": 35,
// CHECK-NEXT:     "col": 3,
// CHECK-NEXT:     "tokLen": 6
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 749,
// CHECK-NEXT:      "col": 1,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 760,
// CHECK-NEXT:      "col": 12,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "name": "Test12",
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "T (T &)"
// CHECK-NEXT:    },
// CHECK-NEXT:    "inner": [
// CHECK-NEXT:     {
// CHECK-NEXT:      "id": "0x{{.*}}",
// CHECK-NEXT:      "kind": "ParmVarDecl",
// CHECK-NEXT:      "loc": {
// CHECK-NEXT:       "offset": 760,
// CHECK-NEXT:       "col": 12,
// CHECK-NEXT:       "tokLen": 1
// CHECK-NEXT:      },
// CHECK-NEXT:      "range": {
// CHECK-NEXT:       "begin": {
// CHECK-NEXT:        "offset": 758,
// CHECK-NEXT:        "col": 10,
// CHECK-NEXT:        "tokLen": 1
// CHECK-NEXT:       },
// CHECK-NEXT:       "end": {
// CHECK-NEXT:        "offset": 759,
// CHECK-NEXT:        "col": 11,
// CHECK-NEXT:        "tokLen": 1
// CHECK-NEXT:       }
// CHECK-NEXT:      },
// CHECK-NEXT:      "type": {
// CHECK-NEXT:       "qualType": "T &"
// CHECK-NEXT:      }
// CHECK-NEXT:     }
// CHECK-NEXT:    ]
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 769,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 37,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 6
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 764,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 782,
// CHECK-NEXT:    "col": 19,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test13",
// CHECK-NEXT:  "mangledName": "_Z6Test13i",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (int)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 779,
// CHECK-NEXT:     "col": 16,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 776,
// CHECK-NEXT:      "col": 13,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 776,
// CHECK-NEXT:      "col": 13,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "CompoundStmt",
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 781,
// CHECK-NEXT:      "col": 18,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 782,
// CHECK-NEXT:      "col": 19,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    }
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }


// CHECK:  "kind": "FunctionDecl",
// CHECK-NEXT:  "loc": {
// CHECK-NEXT:   "offset": 789,
// CHECK-NEXT:   "file": "{{.*}}",
// CHECK-NEXT:   "line": 38,
// CHECK-NEXT:   "col": 6,
// CHECK-NEXT:   "tokLen": 6
// CHECK-NEXT:  },
// CHECK-NEXT:  "range": {
// CHECK-NEXT:   "begin": {
// CHECK-NEXT:    "offset": 784,
// CHECK-NEXT:    "col": 1,
// CHECK-NEXT:    "tokLen": 4
// CHECK-NEXT:   },
// CHECK-NEXT:   "end": {
// CHECK-NEXT:    "offset": 807,
// CHECK-NEXT:    "col": 24,
// CHECK-NEXT:    "tokLen": 1
// CHECK-NEXT:   }
// CHECK-NEXT:  },
// CHECK-NEXT:  "name": "Test14",
// CHECK-NEXT:  "mangledName": "_Z6Test14iz",
// CHECK-NEXT:  "type": {
// CHECK-NEXT:   "qualType": "void (int, ...)"
// CHECK-NEXT:  },
// CHECK-NEXT:  "variadic": true,
// CHECK-NEXT:  "inner": [
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "ParmVarDecl",
// CHECK-NEXT:    "loc": {
// CHECK-NEXT:     "offset": 799,
// CHECK-NEXT:     "col": 16,
// CHECK-NEXT:     "tokLen": 1
// CHECK-NEXT:    },
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 796,
// CHECK-NEXT:      "col": 13,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 796,
// CHECK-NEXT:      "col": 13,
// CHECK-NEXT:      "tokLen": 3
// CHECK-NEXT:     }
// CHECK-NEXT:    },
// CHECK-NEXT:    "type": {
// CHECK-NEXT:     "qualType": "int"
// CHECK-NEXT:    }
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:    "id": "0x{{.*}}",
// CHECK-NEXT:    "kind": "CompoundStmt",
// CHECK-NEXT:    "range": {
// CHECK-NEXT:     "begin": {
// CHECK-NEXT:      "offset": 806,
// CHECK-NEXT:      "col": 23,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     },
// CHECK-NEXT:     "end": {
// CHECK-NEXT:      "offset": 807,
// CHECK-NEXT:      "col": 24,
// CHECK-NEXT:      "tokLen": 1
// CHECK-NEXT:     }
// CHECK-NEXT:    }
// CHECK-NEXT:   }
// CHECK-NEXT:  ]
// CHECK-NEXT: }
