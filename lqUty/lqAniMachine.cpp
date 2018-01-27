/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#include "lqAniMachine.h"
#include <QAbstractTransition>

lqAniMachine::lqAniMachine(QObject *parent) :
    QStateMachine(parent)
{
    source = new QState(this);
    target = new QState(this);

    animation = new QParallelAnimationGroup(this);
    source->addTransition(target)->addAnimation(animation);

    setInitialState(source);
}

void lqAniMachine::animateTargetProperty(QObject *x, const char *prop, QVariant v)
{
    target->assignProperty(x, prop, v);
    animation->addAnimation(new QPropertyAnimation(x, prop));
}

void lqAniMachine::run(cleanUpState *toCleanup, int msec_duration)
{
    for (int i = 0; i < animation->animationCount(); ++i)
        qobject_cast<QPropertyAnimation*>(animation->animationAt(i))->setDuration(msec_duration);

    // auto delete after animation
    if (!toCleanup)
        toCleanup = new cleanUpState(this);
    target->addTransition(animation, SIGNAL(finished()), toCleanup);
}

void lqAniMachine::cleanUpState::onEntry(QEvent *event)
{
    QFinalState::onEntry(event);
    machine()->deleteLater();
}
