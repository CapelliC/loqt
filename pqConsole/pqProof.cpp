/*
    pqConsole    : interfacing SWI-Prolog and Qt

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

#include "PREDICATE.h"
#include "pqProof.h"
#include "pqProofView.h"

#include <QTime>
#include <QStack>
#include <QMdiArea>
#include <QApplication>

#include <QDebug>

/** from trace_interception.pl */
predicate5(goal_source_position)

/** from http://www.swi-prolog.org/pldoc/man?section=manipstack */
predicate1(prolog_current_frame)
predicate1(prolog_current_choice)
predicate3(prolog_frame_attribute)
predicate3(prolog_choice_attribute)

predicate2(set_prolog_flag)
predicate0(trace)

template <class W>
static W* searchApplicationNestedWidget() {
    for (auto t : QApplication::topLevelWidgets()) {
        QStack<QWidget*> stack;
        stack.push(t);
        while (!stack.isEmpty()) {
            auto s = stack.pop();
            for (auto c: s->children()) {
                if (auto m = qobject_cast<W*>(c))
                    return m;
                if (auto z = qobject_cast<QWidget*>(c))
                    stack.push(z);
            }
        }
    }
    return 0;
}

pqProof::pqProof(QObject *parent) : QObject(parent) {

}
void pqProof::installView() {
    auto v = new pqProofView;
    if (auto a = searchApplicationNestedWidget<QMdiArea>())
        a->addSubWindow(v);
    v->show();
    set_prolog_flag(A("pq_tracer"), A("true"));
    trace();
}

PREDICATE(pq_trace, 0) {
    Q_UNUSED(PL_av)
    pqProof::installView();
    return true;
}
PREDICATE(pq_trace, 1) {
    qDebug() << QTime::currentTime() << t2w(PL_A1);
    return TRUE;
}
PREDICATE(pq_trace_interception, 4) {
    const PlTerm
        &Port = PL_A1,
        &Frame = PL_A2,
        &Choice = PL_A3;
    qDebug() << "pq_trace_interception" << t2w(Port) << t2w(Frame) << t2w(Choice);
    /*
    PlTerm Action = PL_A4;
    foreach(t_callback c, debug_callbacks)
        if (c.second(c.first, Port, Frame, Choice, Action)) {
            qDebug() << "action:" << t2w(Action);
            return TRUE;
            //break;
        }
    //qDebug() << "false";
    */
    return FALSE;
}
