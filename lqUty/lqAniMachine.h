/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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

#ifndef LQANIMACHINE_H
#define LQANIMACHINE_H

#include <QPointer>
#include <QFinalState>
#include <QStateMachine>
#include <QPropertyAnimation>
#include <QParallelAnimationGroup>

#include "lqUty_global.h"

//! 2 state animation - basic boilerplate
class LQUTYSHARED_EXPORT lqAniMachine : public QStateMachine
{
    Q_OBJECT

public:

    //! warning: delete machine utility - use with care
    class LQUTYSHARED_EXPORT cleanUpState : public QFinalState {
    public:
        cleanUpState(QStateMachine *machine) : QFinalState(machine) {}
    protected:
        void onEntry(QEvent *event);
    };

    //! parallel animate between 2 states
    explicit lqAniMachine(QObject *parent = 0);

    void animateTargetProperty(QObject *X, const char *prop, QVariant v);

    //! add animations here
    QPointer<QParallelAnimationGroup> animation;

    //! after setup...
    void run(cleanUpState *toCleanup = 0,  int msec_duration = 1000);

    //! could add some state transition
    QPointer<QState> source, target;

signals:
    
public slots:

private:
};

#endif // LQANIMACHINE_H
