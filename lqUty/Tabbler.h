/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef TABBLER_H
#define TABBLER_H

#include <QPointer>
#include <QTextEdit>
#include <QTextFrame>
#include <QTextTable>

/** utility to handle tables in a QTextEdit view
 */
namespace Tabbler {

    typedef QStringList Row;
    typedef QList<Row> Tbl;

    /** attribute for cells */
    inline QTextCharFormat bold() {
        QTextCharFormat cf;
        cf.setFontWeight(QFont::Bold);
        return cf;
    }

    /** get the cursor with cell contents selected */
    inline QTextCursor selectCell(QTextTableCell c) {
        auto b = c.firstCursorPosition();
        b.setPosition(c.lastPosition(), b.KeepAnchor);
        return b;
    }

    /** put text in a row
     *  the header get bold() format
     */
    inline void addRow(QTextTable *tab, int r, Row row, bool header = true) {
        QTextCharFormat cf;
        if (r == 0 && header)
            cf = bold();
        for (int c = 0; c < row.count(); ++c) {
            QTextCursor cursor = tab->cellAt(r, c).firstCursorPosition();
            if (r == 0 && header)
                cursor.insertText(row[c], cf);
            else
                selectCell(tab->cellAt(r, c)).insertHtml(row[c]);
        }
    }

    /** peek the text from cell */
    inline QString textAt(QTextTableCell c) {
        return selectCell(c).selectedText();
    }

    /** transform the text in cell to int. Should add some validation */
    inline int intAt(QTextTableCell c) {
        return textAt(c).toInt();
    }

    /** get last row' cell of table */
    inline QTextTableCell lastCell(QTextTable *t, int col = 0) {
        return t->cellAt(t->rows() - 1, col);
    }

    /** change the background color */
    inline void setCellBackground(QTextTableCell c, QBrush b) {
        QTextCharFormat f;
        f.setBackground(b);
        selectCell(c).setCharFormat(f);
    }

    /** arrange the set of common operations required to append
     *  a fully formatted table into a QTextEdit
     */
    struct TblBuild {

        //! memo the build position
        TblBuild(QTextEdit *te) : count(0) {
            cursor = te->textCursor();
            topFrame = cursor.currentFrame();
        }

        //! syntax sugar
        QTextTable* operator +=(const Tbl &y) { return addTable(y); }

        /** sequentially adding a table
         *  put a break between sequence to get tables placed down in layout
         */
        QTextTable* addTable(const Tbl &t, bool header = true) {
            cursor.setPosition(topFrame->lastPosition());
            if (count++)
                cursor.insertBlock();

            QTextTableFormat tf;
            tf.setHeaderRowCount(1);
            tf.setAlignment(Qt::AlignHCenter);
            tf.setBorder(0.4);

            QTextTable *tab = cursor.insertTable(t.count(), t[0].count());
            tab->setFormat(tf);

            for (int r = 0; r < t.count(); ++r)
                addRow(tab, r, t[r], header);

            return tab;
        }

        void separator() {
            cursor.setPosition(topFrame->lastPosition());
            //cursor.insertHtml("<hr/><br/> \n");
            cursor.insertHtml("\n<hr/> &nbsp; \n");
        }

        QTextCursor cursor;
        QPointer<QTextFrame> topFrame;
        int count;
    };
}

#endif // TABBLER_H
