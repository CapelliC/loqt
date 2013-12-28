/*
    spqr          : SWI-Prolog Qt Rendering

    Author        : Carlo Capelli
    E-mail        : cc.carlo.cap@gmail.com
    Copyright (C) : 2013, Carlo Capelli

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

#include <QTextCursor>
#include "SimPrologEdit.h"
#include "mainwindow.h"

SimPrologEdit::SimPrologEdit(QWidget *parent) :
    SimPrologEditBase(parent)
{
}

void SimPrologEdit::keyPressEvent(QKeyEvent *e)
{
    switch (e->key()) {

    case Qt::Key_F1: {
        QTextCursor c = textCursor();
        c.select(c.WordUnderCursor);
        QString s = c.selectedText();
        if (s.length())
            for (QWidget *w = this; w; w = w->parentWidget())
                if (MainWindow* m = qobject_cast<MainWindow*>(w)) {
                    m->activate(m->t_helpdoc);
                    m->helpDoc()->setUrl(
                        QString("http://localhost:%1/search?for=%2&in=all&match=summary")
                                .arg(m->DOC_PORT).arg(s));
                    break;
                }
    }   break;

    case Qt::Key_Tab:
        e->ignore();
        return;
    case Qt::Key_Backtab:
        e->ignore();
        return;
    }

    SimPrologEditBase::keyPressEvent(e);
}
