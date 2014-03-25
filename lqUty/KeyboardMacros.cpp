/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#include "KeyboardMacros.h"

KeyboardMacros::KeyboardMacros(QObject *parent) :
    QObject(parent)
{
    lqPreferences p;
    int n = p.beginReadArray("KeyboardMacros");
    for (int i = 0; i < n; ++i) {
        p.setArrayIndex(i);
        QString name = p.value("name").toString();
        QStringList strokes = p.value("strokes").toStringList();
        macro m;
        foreach(QString s, strokes)
            m << s2e(s);
        macros[name] = m;
    }
    p.endArray();
}

KeyboardMacros::~KeyboardMacros()
{
    lqPreferences p;

    p.beginWriteArray("KeyboardMacros");
    int i = 0;
    foreach(QString name, macros.keys()) {
        p.setArrayIndex(i);
        p.setValue("name", name);
        QStringList s;
        foreach (QKeyEvent e, macros[name])
            s << e2s(e);
        p.setValue("strokes", s);
    }
    p.endArray();
}

void KeyboardMacros::startRecording(EditInterface *ei)
{

}

void KeyboardMacros::doneRecording(EditInterface *ei)
{

}

void KeyboardMacros::startPlayback(EditInterface *ei)
{

}

void KeyboardMacros::startPlayback(QString name, EditInterface *ei)
{

}

QStringList KeyboardMacros::e2l(const QKeyEvent &e)
{
    QStringList l;
    l << QString::number(e.type()) << QString::number(e.key()) << QString::number(e.modifiers());
    return l;
}

QKeyEvent KeyboardMacros::l2e(const QStringList &l)
{
    int type = l[0].toInt(), key = l[1].toInt(), modif = l[2].toInt();
    return QKeyEvent(QEvent::Type(type), key, Qt::KeyboardModifier(modif));
}
