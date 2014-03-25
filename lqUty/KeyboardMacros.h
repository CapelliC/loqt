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

#ifndef KEYBOARDMACROS_H
#define KEYBOARDMACROS_H

#include "EditInterface.h"
#include "lqPreferences.h"
#include <QKeyEvent>

/** helper class to record and playback macros
 */
class LQUTYSHARED_EXPORT KeyboardMacros : public QObject
{
    Q_OBJECT
public:

    explicit KeyboardMacros(QObject *parent = 0);
    ~KeyboardMacros();

    void startRecording(EditInterface *ei);
    void doneRecording(EditInterface *ei);
    void startPlayback(EditInterface *ei);
    void startPlayback(QString name, EditInterface *ei);

    static QStringList e2l(const QKeyEvent &e);
    static QKeyEvent l2e(const QStringList &l);
    static QString e2s(const QKeyEvent &e) { return e2l(e).join(","); }
    static QKeyEvent s2e(const QString &s) { return l2e(s.split(",")); }

signals:

public slots:

private:

    typedef QList<QKeyEvent> macro;
    QMap<QString, macro> macros;

    QString lastRecorded;
};

#endif // KEYBOARDMACROS_H
