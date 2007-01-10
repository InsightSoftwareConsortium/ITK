MyVertexType _bottom_right[] = {MyVertexType(11.5, 8), MyVertexType(12, 7.5)};
MyVertexListType bottom_right(_bottom_right, _bottom_right + 2);
MyVertexListType rev_bottom_right(bottom_right.rbegin(), bottom_right.rend());

MyVertexType _bottom_left[] = {MyVertexType(0, 7.5), MyVertexType(0.5, 8)};
MyVertexListType bottom_left(_bottom_left, _bottom_left + 2);
MyVertexListType rev_bottom_left(bottom_left.rbegin(), bottom_left.rend());

MyVertexType _row2_col4[] = {MyVertexType(11, 6.5), MyVertexType(10, 6.5), MyVertexType(9.5, 6), MyVertexType(10, 5.5), MyVertexType(10.5, 5), MyVertexType(11, 4.5), MyVertexType(11.5, 5), MyVertexType(11.5, 6), MyVertexType(11, 6.5)};
MyVertexListType row2_col4(_row2_col4, _row2_col4 + 9);
MyVertexListType rev_row2_col4(row2_col4.rbegin(), row2_col4.rend());

MyVertexType _row2_col3[] = {MyVertexType(8, 6.5), MyVertexType(7, 6.5), MyVertexType(6.5, 6), MyVertexType(6.5, 5), MyVertexType(7, 4.5), MyVertexType(7.5, 5), MyVertexType(8, 5.5), MyVertexType(8.5, 6), MyVertexType(8, 6.5)};
MyVertexListType row2_col3(_row2_col3, _row2_col3 + 9);
MyVertexListType rev_row2_col3(row2_col3.rbegin(), row2_col3.rend());

MyVertexType _row2_col2[] = {MyVertexType(5, 6.5), MyVertexType(4.5, 6), MyVertexType(4, 5.5), MyVertexType(3.5, 5), MyVertexType(4, 4.5), MyVertexType(5, 4.5), MyVertexType(5.5, 5), MyVertexType(5.5, 6), MyVertexType(5, 6.5)};
MyVertexListType row2_col2(_row2_col2, _row2_col2 + 9);
MyVertexListType rev_row2_col2(row2_col2.rbegin(), row2_col2.rend());

MyVertexType _row2_col1[] = {MyVertexType(1, 6.5), MyVertexType(0.5, 6), MyVertexType(0.5, 5), MyVertexType(1, 4.5), MyVertexType(2, 4.5), MyVertexType(2.5, 5), MyVertexType(2, 5.5), MyVertexType(1.5, 6), MyVertexType(1, 6.5)};
MyVertexListType row2_col1(_row2_col1, _row2_col1 + 9);
MyVertexListType rev_row2_col1(row2_col1.rbegin(), row2_col1.rend());

MyVertexType _row1_col4_middle[] = {MyVertexType(10, 3.5), MyVertexType(9.5, 3), MyVertexType(10, 2.5), MyVertexType(10.5, 3), MyVertexType(10, 3.5)};
MyVertexListType row1_col4_middle(_row1_col4_middle, _row1_col4_middle + 5);
MyVertexListType rev_row1_col4_middle(row1_col4_middle.rbegin(), row1_col4_middle.rend());

MyVertexType _row1_col2[] = {MyVertexType(5, 3.5), MyVertexType(4, 3.5), MyVertexType(3.5, 3), MyVertexType(4, 2.5), MyVertexType(5, 2.5), MyVertexType(5.5, 3), MyVertexType(5, 3.5)};
MyVertexListType row1_col2(_row1_col2, _row1_col2 + 7);
MyVertexListType rev_row1_col2(row1_col2.rbegin(), row1_col2.rend());

MyVertexType _row1_col1[] = {MyVertexType(2, 3.5), MyVertexType(1.5, 3), MyVertexType(2, 2.5), MyVertexType(2.5, 3), MyVertexType(2, 3.5)};
MyVertexListType row1_col1(_row1_col1, _row1_col1 + 5);
MyVertexListType rev_row1_col1(row1_col1.rbegin(), row1_col1.rend());

MyVertexType _row1_col4_right[] = {MyVertexType(11, 2.5), MyVertexType(10.5, 2), MyVertexType(11, 1.5), MyVertexType(11.5, 2), MyVertexType(11, 2.5)};
MyVertexListType row1_col4_right(_row1_col4_right, _row1_col4_right + 5);
MyVertexListType rev_row1_col4_right(row1_col4_right.rbegin(), row1_col4_right.rend());

MyVertexType _row1_col4_left[] = {MyVertexType(9, 2.5), MyVertexType(8.5, 2), MyVertexType(9, 1.5), MyVertexType(9.5, 2), MyVertexType(9, 2.5)};
MyVertexListType row1_col4_left(_row1_col4_left, _row1_col4_left + 5);
MyVertexListType rev_row1_col4_left(row1_col4_left.rbegin(), row1_col4_left.rend());

MyVertexType _row1_col3[] = {MyVertexType(7, 3.5), MyVertexType(6.5, 3), MyVertexType(6.5, 2), MyVertexType(7, 1.5), MyVertexType(7.5, 2), MyVertexType(7.5, 3), MyVertexType(7, 3.5)};
MyVertexListType row1_col3(_row1_col3, _row1_col3 + 7);
MyVertexListType rev_row1_col3(row1_col3.rbegin(), row1_col3.rend());

MyVertexType _top_right[] = {MyVertexType(12, 0.5), MyVertexType(11.5, 0)};
MyVertexListType top_right(_top_right, _top_right + 2);
MyVertexListType rev_top_right(top_right.rbegin(), top_right.rend());

MyVertexType _top_left[] = {MyVertexType(1.5, 0), MyVertexType(1, 0.5), MyVertexType(0.5, 1), MyVertexType(0, 1.5)};
MyVertexListType top_left(_top_left, _top_left + 4);
MyVertexListType rev_top_left(top_left.rbegin(), top_left.rend());

MyVertexType _top_left_cropped[] = {MyVertexType(0.5, 1), MyVertexType(0, 1.5)};
MyVertexListType top_left_cropped(_top_left_cropped, _top_left_cropped + 2);
MyVertexListType rev_top_left_cropped(top_left_cropped.rbegin(), top_left_cropped.rend());

MyVertexType _row1_col4_all[] = {MyVertexType(10, 3.5), MyVertexType(9.5, 3), MyVertexType(9, 2.5), MyVertexType(8.5, 2), MyVertexType(9, 1.5), MyVertexType(9.5, 2), MyVertexType(10, 2.5), MyVertexType(10.5, 2), MyVertexType(11, 1.5), MyVertexType(11.5, 2), MyVertexType(11, 2.5), MyVertexType(10.5, 3), MyVertexType(10, 3.5)};
MyVertexListType row1_col4_all(_row1_col4_all, _row1_col4_all + 13);
MyVertexListType rev_row1_col4_all(row1_col4_all.rbegin(), row1_col4_all.rend());

MyVertexListType _edco[] = {top_left, top_right, row1_col3, row1_col4_left, row1_col4_right, row1_col1, row1_col2, row1_col4_middle, row2_col1, row2_col2, row2_col3, row2_col4, bottom_left, bottom_right};
MyVertexListList expected_disconnected_clockwise_outputs(_edco, _edco + 14);

MyVertexListType _edcco[] = {rev_top_left, rev_top_right, rev_row1_col3, rev_row1_col4_left, rev_row1_col4_right, rev_row1_col1, rev_row1_col2, rev_row1_col4_middle, rev_row2_col1, rev_row2_col2, rev_row2_col3, rev_row2_col4, rev_bottom_left, rev_bottom_right};
MyVertexListList expected_disconnected_counterclockwise_outputs(_edcco, _edcco + 14);

MyVertexListType _ecco[] = {top_left, top_right, row1_col3, row1_col4_all, row1_col1, row1_col2, row2_col1, row2_col2, row2_col3, row2_col4, bottom_left, bottom_right};
MyVertexListList expected_connected_clockwise_outputs(_ecco, _ecco + 12);

MyVertexListType _edcro[] = {top_left_cropped, row1_col3, row1_col4_left, row1_col4_right, row1_col1, row1_col2, row1_col4_middle, row2_col1, row2_col2, row2_col3, row2_col4};
MyVertexListList expected_disconnected_clockwise_cropped_outputs(_edcro, _edcro + 11);
