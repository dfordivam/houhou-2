{-# LANGUAGE OverloadedStrings #-}
module Radicals where

import Protolude
import Common
import qualified Data.Map as Map

type RadicalTable = Map RadicalId RadicalDetails

radicalTable :: RadicalTable
radicalTable = Map.fromList $
  [(RadicalId 1, RadicalDetails "｜" "")
  ,(RadicalId 2, RadicalDetails "一" "")
  ,(RadicalId 3, RadicalDetails "口" "")
  ,(RadicalId 4, RadicalDetails "女" "")
  ,(RadicalId 5, RadicalDetails "土" "")
  ,(RadicalId 6, RadicalDetails "亅" "")
  ,(RadicalId 7, RadicalDetails "阡" "")
  ,(RadicalId 8, RadicalDetails "衣" "")
  ,(RadicalId 9, RadicalDetails "亠" "")
  ,(RadicalId 10, RadicalDetails "心" "")
  ,(RadicalId 11, RadicalDetails "爪" "")
  ,(RadicalId 12, RadicalDetails "冖" "")
  ,(RadicalId 13, RadicalDetails "夂" "")
  ,(RadicalId 14, RadicalDetails "矢" "")
  ,(RadicalId 15, RadicalDetails "厶" "")
  ,(RadicalId 16, RadicalDetails "扎" "")
  ,(RadicalId 17, RadicalDetails "个" "")
  ,(RadicalId 18, RadicalDetails "込" "")
  ,(RadicalId 19, RadicalDetails "二" "")
  ,(RadicalId 20, RadicalDetails "人" "")
  ,(RadicalId 21, RadicalDetails "大" "")
  ,(RadicalId 22, RadicalDetails "癶" "")
  ,(RadicalId 23, RadicalDetails "艾" "")
  ,(RadicalId 24, RadicalDetails "ノ" "")
  ,(RadicalId 25, RadicalDetails "西" "")
  ,(RadicalId 26, RadicalDetails "禾" "")
  ,(RadicalId 27, RadicalDetails "亀" "")
  ,(RadicalId 28, RadicalDetails "乙" "")
  ,(RadicalId 29, RadicalDetails "勹" "")
  ,(RadicalId 30, RadicalDetails "田" "")
  ,(RadicalId 31, RadicalDetails "至" "")
  ,(RadicalId 32, RadicalDetails "尸" "")
  ,(RadicalId 33, RadicalDetails "汁" "")
  ,(RadicalId 34, RadicalDetails "日" "")
  ,(RadicalId 35, RadicalDetails "九" "")
  ,(RadicalId 36, RadicalDetails "韋" "")
  ,(RadicalId 37, RadicalDetails "戸" "")
  ,(RadicalId 38, RadicalDetails "魚" "")
  ,(RadicalId 39, RadicalDetails "彡" "")
  ,(RadicalId 40, RadicalDetails "杰" "")
  ,(RadicalId 41, RadicalDetails "十" "")
  ,(RadicalId 42, RadicalDetails "辛" "")
  ,(RadicalId 43, RadicalDetails "木" "")
  ,(RadicalId 44, RadicalDetails "立" "")
  ,(RadicalId 45, RadicalDetails "厂" "")
  ,(RadicalId 46, RadicalDetails "斗" "")
  ,(RadicalId 47, RadicalDetails "及" "")
  ,(RadicalId 48, RadicalDetails "夕" "")
  ,(RadicalId 49, RadicalDetails "卩" "")
  ,(RadicalId 50, RadicalDetails "宀" "")
  ,(RadicalId 51, RadicalDetails "目" "")
  ,(RadicalId 52, RadicalDetails "虫" "")
  ,(RadicalId 53, RadicalDetails "亡" "")
  ,(RadicalId 54, RadicalDetails "食" "")
  ,(RadicalId 55, RadicalDetails "糸" "")
  ,(RadicalId 56, RadicalDetails "幺" "")
  ,(RadicalId 57, RadicalDetails "小" "")
  ,(RadicalId 58, RadicalDetails "儿" "")
  ,(RadicalId 59, RadicalDetails "卜" "")
  ,(RadicalId 60, RadicalDetails "戈" "")
  ,(RadicalId 61, RadicalDetails "米" "")
  ,(RadicalId 62, RadicalDetails "初" "")
  ,(RadicalId 63, RadicalDetails "广" "")
  ,(RadicalId 64, RadicalDetails "音" "")
  ,(RadicalId 65, RadicalDetails "門" "")
  ,(RadicalId 66, RadicalDetails "革" "")
  ,(RadicalId 67, RadicalDetails "丶" "")
  ,(RadicalId 68, RadicalDetails "ヨ" "")
  ,(RadicalId 69, RadicalDetails "化" "")
  ,(RadicalId 70, RadicalDetails "囗" "")
  ,(RadicalId 71, RadicalDetails "井" "")
  ,(RadicalId 72, RadicalDetails "弓" "")
  ,(RadicalId 73, RadicalDetails "示" "")
  ,(RadicalId 74, RadicalDetails "寸" "")
  ,(RadicalId 75, RadicalDetails "忙" "")
  ,(RadicalId 76, RadicalDetails "隹" "")
  ,(RadicalId 77, RadicalDetails "勿" "")
  ,(RadicalId 78, RadicalDetails "ハ" "")
  ,(RadicalId 79, RadicalDetails "月" "")
  ,(RadicalId 80, RadicalDetails "言" "")
  ,(RadicalId 81, RadicalDetails "貝" "")
  ,(RadicalId 82, RadicalDetails "匚" "")
  ,(RadicalId 83, RadicalDetails "邦" "")
  ,(RadicalId 84, RadicalDetails "石" "")
  ,(RadicalId 85, RadicalDetails "士" "")
  ,(RadicalId 86, RadicalDetails "匕" "")
  ,(RadicalId 87, RadicalDetails "皿" "")
  ,(RadicalId 88, RadicalDetails "尚" "")
  ,(RadicalId 89, RadicalDetails "并" "")
  ,(RadicalId 90, RadicalDetails "免" "")
  ,(RadicalId 91, RadicalDetails "欠" "")
  ,(RadicalId 92, RadicalDetails "冫" "")
  ,(RadicalId 93, RadicalDetails "干" "")
  ,(RadicalId 94, RadicalDetails "王" "")
  ,(RadicalId 95, RadicalDetails "元" "")
  ,(RadicalId 96, RadicalDetails "鳥" "")
  ,(RadicalId 97, RadicalDetails "羽" "")
  ,(RadicalId 98, RadicalDetails "雨" "")
  ,(RadicalId 99, RadicalDetails "見" "")
  ,(RadicalId 100, RadicalDetails "穴" "")
  ,(RadicalId 101, RadicalDetails "臼" "")
  ,(RadicalId 102, RadicalDetails "冂" "")
  ,(RadicalId 103, RadicalDetails "虍" "")
  ,(RadicalId 104, RadicalDetails "艮" "")
  ,(RadicalId 105, RadicalDetails "買" "")
  ,(RadicalId 106, RadicalDetails "又" "")
  ,(RadicalId 107, RadicalDetails "老" "")
  ,(RadicalId 108, RadicalDetails "牙" "")
  ,(RadicalId 109, RadicalDetails "用" "")
  ,(RadicalId 110, RadicalDetails "瓜" "")
  ,(RadicalId 111, RadicalDetails "車" "")
  ,(RadicalId 112, RadicalDetails "耳" "")
  ,(RadicalId 113, RadicalDetails "水" "")
  ,(RadicalId 114, RadicalDetails "頁" "")
  ,(RadicalId 115, RadicalDetails "行" "")
  ,(RadicalId 116, RadicalDetails "彳" "")
  ,(RadicalId 117, RadicalDetails "金" "")
  ,(RadicalId 118, RadicalDetails "攵" "")
  ,(RadicalId 119, RadicalDetails "殳" "")
  ,(RadicalId 120, RadicalDetails "疔" "")
  ,(RadicalId 121, RadicalDetails "几" "")
  ,(RadicalId 122, RadicalDetails "馬" "")
  ,(RadicalId 123, RadicalDetails "走" "")
  ,(RadicalId 124, RadicalDetails "自" "")
  ,(RadicalId 125, RadicalDetails "犬" "")
  ,(RadicalId 126, RadicalDetails "奄" "")
  ,(RadicalId 127, RadicalDetails "止" "")
  ,(RadicalId 128, RadicalDetails "廴" "")
  ,(RadicalId 129, RadicalDetails "火" "")
  ,(RadicalId 130, RadicalDetails "爿" "")
  ,(RadicalId 131, RadicalDetails "犯" "")
  ,(RadicalId 132, RadicalDetails "豕" "")
  ,(RadicalId 133, RadicalDetails "色" "")
  ,(RadicalId 134, RadicalDetails "豆" "")
  ,(RadicalId 135, RadicalDetails "方" "")
  ,(RadicalId 136, RadicalDetails "生" "")
  ,(RadicalId 137, RadicalDetails "力" "")
  ,(RadicalId 138, RadicalDetails "凵" "")
  ,(RadicalId 139, RadicalDetails "黄" "")
  ,(RadicalId 140, RadicalDetails "廾" "")
  ,(RadicalId 141, RadicalDetails "山" "")
  ,(RadicalId 142, RadicalDetails "岡" "")
  ,(RadicalId 143, RadicalDetails "マ" "")
  ,(RadicalId 144, RadicalDetails "牛" "")
  ,(RadicalId 145, RadicalDetails "刀" "")
  ,(RadicalId 146, RadicalDetails "礼" "")
  ,(RadicalId 147, RadicalDetails "竹" "")
  ,(RadicalId 148, RadicalDetails "文" "")
  ,(RadicalId 149, RadicalDetails "手" "")
  ,(RadicalId 150, RadicalDetails "臣" "")
  ,(RadicalId 151, RadicalDetails "角" "")
  ,(RadicalId 152, RadicalDetails "鬼" "")
  ,(RadicalId 153, RadicalDetails "母" "")
  ,(RadicalId 154, RadicalDetails "毋" "")
  ,(RadicalId 155, RadicalDetails "已" "")
  ,(RadicalId 156, RadicalDetails "白" "")
  ,(RadicalId 157, RadicalDetails "比" "")
  ,(RadicalId 158, RadicalDetails "骨" "")
  ,(RadicalId 159, RadicalDetails "里" "")
  ,(RadicalId 160, RadicalDetails "香" "")
  ,(RadicalId 161, RadicalDetails "巾" "")
  ,(RadicalId 162, RadicalDetails "斤" "")
  ,(RadicalId 163, RadicalDetails "刈" "")
  ,(RadicalId 164, RadicalDetails "聿" "")
  ,(RadicalId 165, RadicalDetails "赤" "")
  ,(RadicalId 166, RadicalDetails "子" "")
  ,(RadicalId 167, RadicalDetails "父" "")
  ,(RadicalId 168, RadicalDetails "鬲" "")
  ,(RadicalId 169, RadicalDetails "毛" "")
  ,(RadicalId 170, RadicalDetails "舌" "")
  ,(RadicalId 171, RadicalDetails "歯" "")
  ,(RadicalId 172, RadicalDetails "矛" "")
  ,(RadicalId 173, RadicalDetails "瓦" "")
  ,(RadicalId 174, RadicalDetails "川" "")
  ,(RadicalId 175, RadicalDetails "甘" "")
  ,(RadicalId 176, RadicalDetails "缶" "")
  ,(RadicalId 177, RadicalDetails "舟" "")
  ,(RadicalId 178, RadicalDetails "品" "")
  ,(RadicalId 179, RadicalDetails "支" "")
  ,(RadicalId 180, RadicalDetails "气" "")
  ,(RadicalId 181, RadicalDetails "羊" "")
  ,(RadicalId 182, RadicalDetails "疋" "")
  ,(RadicalId 183, RadicalDetails "氏" "")
  ,(RadicalId 184, RadicalDetails "黍" "")
  ,(RadicalId 185, RadicalDetails "屮" "")
  ,(RadicalId 186, RadicalDetails "入" "")
  ,(RadicalId 187, RadicalDetails "久" "")
  ,(RadicalId 188, RadicalDetails "乃" "")
  ,(RadicalId 189, RadicalDetails "身" "")
  ,(RadicalId 190, RadicalDetails "巨" "")
  ,(RadicalId 191, RadicalDetails "足" "")
  ,(RadicalId 192, RadicalDetails "工" "")
  ,(RadicalId 193, RadicalDetails "耒" "")
  ,(RadicalId 194, RadicalDetails "禹" "")
  ,(RadicalId 195, RadicalDetails "尤" "")
  ,(RadicalId 196, RadicalDetails "舛" "")
  ,(RadicalId 197, RadicalDetails "血" "")
  ,(RadicalId 198, RadicalDetails "鹵" "")
  ,(RadicalId 199, RadicalDetails "玄" "")
  ,(RadicalId 200, RadicalDetails "鼓" "")
  ,(RadicalId 201, RadicalDetails "五" "")
  ,(RadicalId 202, RadicalDetails "彑" "")
  ,(RadicalId 203, RadicalDetails "酉" "")
  ,(RadicalId 204, RadicalDetails "隶" "")
  ,(RadicalId 205, RadicalDetails "高" "")
  ,(RadicalId 206, RadicalDetails "麦" "")
  ,(RadicalId 207, RadicalDetails "黒" "")
  ,(RadicalId 208, RadicalDetails "鹿" "")
  ,(RadicalId 209, RadicalDetails "豸" "")
  ,(RadicalId 210, RadicalDetails "斉" "")
  ,(RadicalId 211, RadicalDetails "巛" "")
  ,(RadicalId 212, RadicalDetails "非" "")
  ,(RadicalId 213, RadicalDetails "冊" "")
  ,(RadicalId 214, RadicalDetails "世" "")
  ,(RadicalId 215, RadicalDetails "青" "")
  ,(RadicalId 216, RadicalDetails "歹" "")
  ,(RadicalId 217, RadicalDetails "也" "")
  ,(RadicalId 218, RadicalDetails "弋" "")
  ,(RadicalId 219, RadicalDetails "爻" "")
  ,(RadicalId 220, RadicalDetails "而" "")
  ,(RadicalId 221, RadicalDetails "釆" "")
  ,(RadicalId 222, RadicalDetails "首" "")
  ,(RadicalId 223, RadicalDetails "尢" "")
  ,(RadicalId 224, RadicalDetails "屯" "")
  ,(RadicalId 225, RadicalDetails "巴" "")
  ,(RadicalId 226, RadicalDetails "辰" "")
  ,(RadicalId 227, RadicalDetails "皮" "")
  ,(RadicalId 228, RadicalDetails "鼠" "")
  ,(RadicalId 229, RadicalDetails "谷" "")
  ,(RadicalId 230, RadicalDetails "竜" "")
  ,(RadicalId 231, RadicalDetails "滴" "")
  ,(RadicalId 232, RadicalDetails "長" "")
  ,(RadicalId 233, RadicalDetails "片" "")
  ,(RadicalId 234, RadicalDetails "鼎" "")
  ,(RadicalId 235, RadicalDetails "肉" "")
  ,(RadicalId 236, RadicalDetails "韭" "")
  ,(RadicalId 237, RadicalDetails "髟" "")
  ,(RadicalId 238, RadicalDetails "飛" "")
  ,(RadicalId 239, RadicalDetails "鼻" "")
  ,(RadicalId 240, RadicalDetails "無" "")
  ,(RadicalId 241, RadicalDetails "風" "")
  ,(RadicalId 242, RadicalDetails "麻" "")
  ,(RadicalId 243, RadicalDetails "面" "")
  ,(RadicalId 244, RadicalDetails "曰" "")
  ,(RadicalId 245, RadicalDetails "无" "")
  ,(RadicalId 246, RadicalDetails "齊" "")
  ,(RadicalId 247, RadicalDetails "ユ" "")
  ,(RadicalId 248, RadicalDetails "邑" "")
  ,(RadicalId 249, RadicalDetails "黽" "")
  ,(RadicalId 250, RadicalDetails "龠" "")
  ,(RadicalId 251, RadicalDetails "鬥" "")
  ,(RadicalId 252, RadicalDetails "鬯" "")
  ,(RadicalId 253, RadicalDetails "黹" "")]
