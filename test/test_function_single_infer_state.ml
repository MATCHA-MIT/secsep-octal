open Type.Single_type_infer
open Type.Smt_emitter
open Type.Single_exp

let salsa20_words_single_infer_state : SingleTypeInfer.t = 
{
	func = [];
	func_type =
		[
			{
				label = "sha512_block_data_order";
				pc = 3;
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
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), [], SingleVar (97));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), [], SingleVar (98));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), [], SingleVar (99));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), [], SingleVar (100));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), [], SingleVar (101));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), [], SingleVar (102));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), [], SingleVar (103));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), [], SingleVar (104));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), [], SingleVar (105));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), [], SingleVar (106));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), [], SingleVar (107));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), [], SingleVar (108));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), [], SingleVar (109));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), [], SingleVar (110));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), [], SingleVar (111));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), [], SingleVar (112));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), [], SingleVar (113));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (114));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (115));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (116));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (117));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (118));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (119));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (120));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (121));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (122));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (123));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (124));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (125));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (126));
							((SingleVar (4), SingleVar (4)), [], SingleTop);
						]);
						(7, [
							((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleTop);
						]);
						(6, [
							((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L)))), [], SingleTop);
						]);
						(-2, [
							((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (640L))), [], SingleTop);
						]);
					]
				;
				flag = (SingleTop, SingleTop);
				branch_hist = [];
				full_not_taken_hist = [];
				constraint_list = [];
				local_var_map = [];
				useful_var = SingleExp.SingleVarSet.of_list [];
				global_var = SingleExp.SingleVarSet.of_list [-2];
			}
			;
			{
				label = ".LFB47";
				pc = 4;
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
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), [], SingleVar (127));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), [], SingleVar (128));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), [], SingleVar (129));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), [], SingleVar (130));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), [], SingleVar (131));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), [], SingleVar (132));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), [], SingleVar (133));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), [], SingleVar (134));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), [], SingleVar (135));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), [], SingleVar (136));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), [], SingleVar (137));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), [], SingleVar (138));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), [], SingleVar (139));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), [], SingleVar (140));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), [], SingleVar (141));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), [], SingleVar (142));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), [], SingleVar (143));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (144));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (145));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (146));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (147));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (148));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (149));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (150));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (151));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (152));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (153));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (154));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (155));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (156));
							((SingleVar (4), SingleVar (4)), [], SingleVar (33));
						]);
						(7, [
							((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (34));
						]);
						(6, [
							((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L)))), [], SingleVar (35));
						]);
						(-2, [
							((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (640L))), [], SingleVar (36));
						]);
					]
				;
				flag = (SingleTop, SingleTop);
				branch_hist = [];
				full_not_taken_hist = [];
				constraint_list = [];
				local_var_map = [];
				useful_var = SingleExp.SingleVarSet.of_list [21; 24];
				global_var = SingleExp.SingleVarSet.of_list [-2];
			}
			;
			{
				label = ".L8";
				pc = 32;
				reg_type = 
					[
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
						SingleVar (52);
					]
				;
				mem_type = 
					[
						(4, [
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), [], SingleVar (157));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), [], SingleVar (158));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), [], SingleVar (159));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), [], SingleVar (160));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), [], SingleVar (161));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), [], SingleVar (162));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), [], SingleVar (163));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), [], SingleVar (164));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), [], SingleVar (165));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), [], SingleVar (166));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), [], SingleVar (167));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), [], SingleVar (168));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), [], SingleVar (169));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), [], SingleVar (170));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), [], SingleVar (171));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), [], SingleVar (172));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), [], SingleVar (173));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (174));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (175));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (176));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (177));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (178));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (179));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (180));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (181));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (182));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (183));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (184));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (185));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (186));
							((SingleVar (4), SingleVar (4)), [], SingleVar (53));
						]);
						(7, [
							((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (54));
						]);
						(6, [
							((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L)))), [], SingleVar (55));
						]);
						(-2, [
							((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (640L))), [], SingleVar (56));
						]);
					]
				;
				flag = (SingleTop, SingleTop);
				branch_hist = [];
				full_not_taken_hist = [];
				constraint_list = [];
				local_var_map = [];
				useful_var = SingleExp.SingleVarSet.of_list [41; 49];
				global_var = SingleExp.SingleVarSet.of_list [-2];
			}
			;
			{
				label = ".L7";
				pc = 566;
				reg_type = 
					[
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
						SingleVar (71);
						SingleVar (72);
					]
				;
				mem_type = 
					[
						(4, [
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), [], SingleVar (187));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), [], SingleVar (188));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), [], SingleVar (189));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), [], SingleVar (190));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), [], SingleVar (191));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), [], SingleVar (192));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), [], SingleVar (193));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), [], SingleVar (194));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), [], SingleVar (195));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), [], SingleVar (196));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), [], SingleVar (197));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), [], SingleVar (198));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), [], SingleVar (199));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), [], SingleVar (200));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), [], SingleVar (201));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), [], SingleVar (202));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), [], SingleVar (203));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (204));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (205));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (206));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (207));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (208));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (209));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (210));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (211));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (212));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (213));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (214));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (215));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (216));
							((SingleVar (4), SingleVar (4)), [], SingleVar (73));
						]);
						(7, [
							((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (74));
						]);
						(6, [
							((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L)))), [], SingleVar (75));
						]);
						(-2, [
							((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (640L))), [], SingleVar (76));
						]);
					]
				;
				flag = (SingleTop, SingleTop);
				branch_hist = [];
				full_not_taken_hist = [];
				constraint_list = [];
				local_var_map = [];
				useful_var = SingleExp.SingleVarSet.of_list [-2; 61; 67; 207; 209; 210];
				global_var = SingleExp.SingleVarSet.of_list [-2];
			}
			;
			{
				label = ".Ret";
				pc = 1421;
				reg_type = 
					[
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
						SingleVar (90);
						SingleVar (91);
						SingleVar (92);
					]
				;
				mem_type = 
					[
						(4, [
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-240L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L))), [], SingleVar (217));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-232L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L))), [], SingleVar (218));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-224L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L))), [], SingleVar (219));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-216L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L))), [], SingleVar (220));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-208L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L))), [], SingleVar (221));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-200L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L))), [], SingleVar (222));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-192L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L))), [], SingleVar (223));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-184L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L))), [], SingleVar (224));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-176L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L))), [], SingleVar (225));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-168L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L))), [], SingleVar (226));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-160L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L))), [], SingleVar (227));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-152L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L))), [], SingleVar (228));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-144L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L))), [], SingleVar (229));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-136L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L))), [], SingleVar (230));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-128L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L))), [], SingleVar (231));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L))), [], SingleVar (232));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-112L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L))), [], SingleVar (233));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-104L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L))), [], SingleVar (234));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-96L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L))), [], SingleVar (235));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-88L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L))), [], SingleVar (236));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-80L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L))), [], SingleVar (237));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-72L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L))), [], SingleVar (238));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-64L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L))), [], SingleVar (239));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-56L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L))), [], SingleVar (240));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-48L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L))), [], SingleVar (241));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-40L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L))), [], SingleVar (242));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-32L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L))), [], SingleVar (243));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-24L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L))), [], SingleVar (244));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-16L)), SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L))), [], SingleVar (245));
							((SingleBExp (SingleAdd, SingleVar (4), SingleConst (-8L)), SingleVar (4)), [], SingleVar (246));
							((SingleVar (4), SingleVar (4)), [], SingleVar (93));
						]);
						(7, [
							((SingleVar (7), SingleBExp (SingleAdd, SingleVar (7), SingleConst (64L))), [], SingleVar (94));
						]);
						(6, [
							((SingleVar (6), SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L)))), [], SingleVar (95));
						]);
						(-2, [
							((SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (640L))), [], SingleVar (96));
						]);
					]
				;
				flag = (SingleTop, SingleTop);
				branch_hist = [];
				full_not_taken_hist = [];
				constraint_list = [];
				local_var_map = [];
				useful_var = SingleExp.SingleVarSet.of_list [80; 81; 82; 89; 90; 91; 92; 94; 95; 96];
				global_var = SingleExp.SingleVarSet.of_list [-2];
			}
			;
		]
		;
	single_subtype =
		[
			{ var_idx = (15, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (14, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (13, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (12, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (5, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (3, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (36, 4); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (35, 4); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (32, 4); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
			{ var_idx = (31, 4); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
			{ var_idx = (30, 4); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
			{ var_idx = (29, 4); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
			{ var_idx = (22, 4); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
			{ var_idx = (20, 4); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
			{ var_idx = (2, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (19, 4); sol = SolSimple (Single (SingleVar (2))); subtype_list = []; supertype_list = [] };
			{ var_idx = (56, 32); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (55, 32); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (186, 32); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
			{ var_idx = (185, 32); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
			{ var_idx = (184, 32); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
			{ var_idx = (183, 32); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
			{ var_idx = (182, 32); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
			{ var_idx = (181, 32); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
			{ var_idx = (6, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (76, 566); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (96, 1421); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (75, 566); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (95, 1421); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (94, 1421); sol = SolSimple (Top); subtype_list = []; supertype_list = [] };
			{ var_idx = (216, 566); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
			{ var_idx = (92, 1421); sol = SolSimple (Single (SingleVar (15))); subtype_list = []; supertype_list = [] };
			{ var_idx = (215, 566); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
			{ var_idx = (91, 1421); sol = SolSimple (Single (SingleVar (14))); subtype_list = []; supertype_list = [] };
			{ var_idx = (214, 566); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
			{ var_idx = (90, 1421); sol = SolSimple (Single (SingleVar (13))); subtype_list = []; supertype_list = [] };
			{ var_idx = (213, 566); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
			{ var_idx = (89, 1421); sol = SolSimple (Single (SingleVar (12))); subtype_list = []; supertype_list = [] };
			{ var_idx = (212, 566); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
			{ var_idx = (82, 1421); sol = SolSimple (Single (SingleVar (5))); subtype_list = []; supertype_list = [] };
			{ var_idx = (81, 1421); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
			{ var_idx = (211, 566); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
			{ var_idx = (80, 1421); sol = SolSimple (Single (SingleVar (3))); subtype_list = []; supertype_list = [] };
			{ var_idx = (180, 32); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
			{ var_idx = (179, 32); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))))); subtype_list = []; supertype_list = [] };
			{ var_idx = (-2, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (210, 566); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
			{ var_idx = (209, 566); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))))); subtype_list = []; supertype_list = [] };
			{ var_idx = (207, 566); sol = SolCond (1412, Range (SingleVar (6), SingleBExp (SingleAdd, SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))), SingleConst (-128L)), 128L), Range (SingleVar (6), SingleBExp (SingleAdd, SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))), SingleConst (-256L)), 128L), Single (SingleBExp (SingleAdd, SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))), SingleConst (-128L)))); subtype_list = []; supertype_list = [] };
			{ var_idx = (67, 566); sol = SolCond (1380, Range (SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (384L)), 128L), Range (SingleVar (-2), SingleBExp (SingleAdd, SingleVar (-2), SingleConst (256L)), 128L), Single (SingleBExp (SingleAdd, SingleVar (-2), SingleConst (384L)))); subtype_list = []; supertype_list = [] };
			{ var_idx = (23, 4); sol = SolSimple (Single (SingleVar (6))); subtype_list = []; supertype_list = [] };
			{ var_idx = (49, 32); sol = SolSimple (Range (SingleVar (6), SingleBExp (SingleAdd, SingleBExp (SingleAdd, SingleVar (6), SingleBExp (SingleMul, SingleVar (2), SingleConst (128L))), SingleConst (-128L)), 128L)); subtype_list = []; supertype_list = [] };
			{ var_idx = (61, 566); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
			{ var_idx = (41, 32); sol = SolSimple (Single (SingleBExp (SingleAdd, SingleVar (4), SingleConst (-120L)))); subtype_list = []; supertype_list = [] };
			{ var_idx = (7, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (24, 4); sol = SolSimple (Single (SingleVar (7))); subtype_list = []; supertype_list = [] };
			{ var_idx = (4, 3); sol = SolNone; subtype_list = []; supertype_list = [] };
			{ var_idx = (21, 4); sol = SolSimple (Single (SingleVar (4))); subtype_list = []; supertype_list = [] };
		]
		;
	next_var = SingleTop;
	input_var_set = SingleExp.SingleVarSet.of_list [-2; -1; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16];
	smt_ctx = SmtEmitter.init_smt_ctx ();
}

  