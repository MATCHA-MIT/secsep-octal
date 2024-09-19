open Type.Single_type_infer
open Type.Smt_emitter
open Type.Single_exp

let salsa20_words_single_infer_state : SingleTypeInfer.t = 
  {
    func = [];
    func_type =
      [
        {
          label = "salsa20_words";
          pc = 5;
          reg_type = 
            [
              SingleVar (0);
              SingleVar (1);
              SingleVar (2);
              SingleVar (3);
              SingleVar (4);
              SingleVar (5);
              SingleVar (6);
              SingleVar (7);
              SingleVar (8);
              SingleVar (9);
              SingleVar (10);
              SingleVar (11);
              SingleVar (12);
              SingleVar (13);
              SingleVar (14);
              SingleVar (15);
            ]
          ;
          mem_type = 
            [
              (4, [
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), [], SingleVar (93));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (94));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), [], SingleVar (95));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (96));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), [], SingleVar (97));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (98));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), [], SingleVar (99));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (100));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), [], SingleVar (101));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (102));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (103));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (104));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (105));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (106));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (107));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (108));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (109));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (110));
                ((SingleVar (4), SingleVar (4)), [], SingleTop);
              ]);
              (7, [
                ((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleTop);
              ]);
              (6, [
                ((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), [], SingleTop);
              ]);
            ]
          ;
          flag = (SingleTop, SingleTop);
          branch_hist = [];
          full_not_taken_hist = [];
          constraint_list = [];
          local_var_map = [];
          useful_var = SingleExp.SingleVarSet.of_list [];
          global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
        }
        ;
        {
          label = ".LFB2";
          pc = 6;
          reg_type = 
            [
              SingleVar (17);
              SingleVar (18);
              SingleVar (19);
              SingleVar (20);
              SingleVar (21);
              SingleVar (22);
              SingleVar (23);
              SingleVar (24);
              SingleVar (25);
              SingleVar (26);
              SingleVar (27);
              SingleVar (28);
              SingleVar (29);
              SingleVar (30);
              SingleVar (31);
              SingleVar (32);
            ]
          ;
          mem_type = 
            [
              (4, [
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), [], SingleVar (111));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (112));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), [], SingleVar (113));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (114));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), [], SingleVar (115));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (116));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), [], SingleVar (117));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (118));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), [], SingleVar (119));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (120));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (121));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (122));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (123));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (124));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (125));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (126));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (127));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (128));
                ((SingleVar (4), SingleVar (4)), [], SingleVar (33));
              ]);
              (7, [
                ((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (34));
              ]);
              (6, [
                ((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), [], SingleVar (35));
              ]);
            ]
          ;
          flag = (SingleTop, SingleTop);
          branch_hist = [];
          full_not_taken_hist = [];
          constraint_list = [];
          local_var_map = [];
          useful_var = SingleExp.SingleVarSet.of_list [21; 23];
          global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
        }
        ;
        {
          label = ".L2";
          pc = 45;
          reg_type = 
            [
              SingleVar (36);
              SingleVar (37);
              SingleVar (38);
              SingleVar (39);
              SingleVar (40);
              SingleVar (41);
              SingleVar (42);
              SingleVar (43);
              SingleVar (44);
              SingleVar (45);
              SingleVar (46);
              SingleVar (47);
              SingleVar (48);
              SingleVar (49);
              SingleVar (50);
              SingleVar (51);
            ]
          ;
          mem_type = 
            [
              (4, [
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), [], SingleVar (129));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (130));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), [], SingleVar (131));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (132));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), [], SingleVar (133));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (134));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), [], SingleVar (135));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (136));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), [], SingleVar (137));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (138));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (139));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (140));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (141));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (142));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (143));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (144));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (145));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (146));
                ((SingleVar (4), SingleVar (4)), [], SingleVar (52));
              ]);
              (7, [
                ((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (53));
              ]);
              (6, [
                ((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), [], SingleVar (54));
              ]);
            ]
          ;
          flag = (SingleTop, SingleTop);
          branch_hist = [];
          full_not_taken_hist = [];
          constraint_list = [];
          local_var_map = [];
          useful_var = SingleExp.SingleVarSet.of_list [40; 139; 140];
          global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
        }
        ;
        {
          label = ".LFE2";
          pc = 221;
          reg_type = 
            [
              SingleVar (55);
              SingleVar (56);
              SingleVar (57);
              SingleVar (58);
              SingleVar (59);
              SingleVar (60);
              SingleVar (61);
              SingleVar (62);
              SingleVar (63);
              SingleVar (64);
              SingleVar (65);
              SingleVar (66);
              SingleVar (67);
              SingleVar (68);
              SingleVar (69);
              SingleVar (70);
            ]
          ;
          mem_type = 
            [
              (4, [
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), [], SingleVar (147));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (148));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), [], SingleVar (149));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (150));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), [], SingleVar (151));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (152));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), [], SingleVar (153));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (154));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), [], SingleVar (155));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (156));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (157));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (158));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (159));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (160));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (161));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (162));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (163));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (164));
                ((SingleVar (4), SingleVar (4)), [], SingleVar (71));
              ]);
              (7, [
                ((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (72));
              ]);
              (6, [
                ((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), [], SingleVar (73));
              ]);
            ]
          ;
          flag = (SingleTop, SingleTop);
          branch_hist = [];
          full_not_taken_hist = [];
          constraint_list = [];
          local_var_map = [];
          useful_var = SingleExp.SingleVarSet.of_list [];
          global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
        }
        ;
        {
          label = ".Ret";
          pc = 221;
          reg_type = 
            [
              SingleVar (74);
              SingleVar (75);
              SingleVar (76);
              SingleVar (77);
              SingleVar (78);
              SingleVar (79);
              SingleVar (80);
              SingleVar (81);
              SingleVar (82);
              SingleVar (83);
              SingleVar (84);
              SingleVar (85);
              SingleVar (86);
              SingleVar (87);
              SingleVar (88);
              SingleVar (89);
            ]
          ;
          mem_type = 
            [
              (4, [
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L))), [], SingleVar (165));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-100L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (166));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L))), [], SingleVar (167));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-92L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (168));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L))), [], SingleVar (169));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-84L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (170));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L))), [], SingleVar (171));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-76L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (172));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L))), [], SingleVar (173));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-68L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (174));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (175));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (176));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (177));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (178));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (179));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (180));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (181));
                ((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (182));
                ((SingleVar (4), SingleVar (4)), [], SingleVar (90));
              ]);
              (7, [
                ((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (91));
              ]);
              (6, [
                ((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleConst (64L))), [], SingleVar (92));
              ]);
            ]
          ;
          flag = (SingleTop, SingleTop);
          branch_hist = [];
          full_not_taken_hist = [];
          constraint_list = [];
          local_var_map = [];
          useful_var = SingleExp.SingleVarSet.of_list [77; 78; 79; 86; 87; 88; 89; 91; 92];
          global_var = SingleExp.SingleVarSet.of_list [-4; -3; -2];
        }
        ;
      ]
      ;
    single_subtype =
      [
        { var_idx = (15, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (14, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (13, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (12, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (5, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (3, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (35, 6); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
        { var_idx = (32, 6); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
        { var_idx = (31, 6); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
        { var_idx = (30, 6); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
        { var_idx = (29, 6); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
        { var_idx = (22, 6); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
        { var_idx = (20, 6); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
        { var_idx = (7, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (54, 45); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
        { var_idx = (92, 221); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
        { var_idx = (91, 221); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
        { var_idx = (146, 45); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
        { var_idx = (89, 221); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
        { var_idx = (145, 45); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
        { var_idx = (88, 221); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
        { var_idx = (144, 45); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
        { var_idx = (87, 221); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
        { var_idx = (143, 45); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
        { var_idx = (86, 221); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
        { var_idx = (142, 45); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
        { var_idx = (79, 221); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
        { var_idx = (78, 221); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
        { var_idx = (141, 45); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
        { var_idx = (77, 221); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
        { var_idx = (24, 6); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
        { var_idx = (140, 45); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
        { var_idx = (139, 45); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
        { var_idx = (40, 45); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)))); subtype_list = []; supertype_list = [] };
        { var_idx = (6, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (23, 6); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
        { var_idx = (4, 5); sol = SolNone; subtype_list = []; supertype_list = [] };
        { var_idx = (21, 6); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
      ]
      ;
    next_var = SingleTop;
    input_var_set = SingleExp.SingleVarSet.of_list [-4; -3; -2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
    smt_ctx = SmtEmitter.init_smt_ctx ();
  }
  