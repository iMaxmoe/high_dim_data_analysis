# -*- coding: utf-8 -*-
"""
Created on Sun Feb 10 10:29:52 2019

@author: Maxmoe
"""

import numpy as np
import pandas as pd
from scipy import optimize

class POET:
    def poet(self, Y, K=-np.inf, C=-np.inf, thres='hard', matrix='cor'):
        
        def POETCmin(Y,K,thres,matrix='cor'):
            p = Y.shape[0]
            n = Y.shape[1]

            def min_eig(Y,K,C,thres,matrix):
                SigmaU = poet(Y,K,C,thres,matrix)['SigmaU']
                f = min(np.linalg.eigvals(SigmaU))
                return f

            def f(x):
                return min_eig(Y,K,x,thres,matrix)

            if f(50)*f(-50)<0:
                roots = scipy.optimize.fsolve(f,[-50,50])
                result = max(0,roots)
            else:
                result = 0
            return result
    
        p = Y.shape[0] # Number of attributes
        n = Y.shape[1] # Number of observations
        Y = Y.sub(Y.mean(axis=1),axis=0) # De-mean
        Y = Y.values
        
        if K == np.inf:
            # TO-DO
            pass
        elif K > 0:
            # Dd: eignevalues, V: eigenvectors
            Dd, V = np.linalg.eig(Y.T @ Y)
            F = np.sqrt(n)*V[:,:K]  # n by k
            LamPCA = Y @ F / n  # p by k
            uhat = Y - LamPCA @ F.T  # p by n
            Lowrank = LamPCA @ LamPCA.T # p by p
            rate = 1/np.sqrt(p)+np.sqrt((np.log(p))/n)
        else: # Sigma_y itself is sparse
            uhat = Y
            rate = np.sqrt((np.log(p))/n)
            Lowrank = np.zeros([p,p])
        
        SuPCA = uhat @ uhat.T / n  # p by p
        SuDiag = np.diag(np.diag(SuPCA))
        if matrix == 'cor':
            R = np.linalg.inv(SuDiag**(1/2)) @ SuPCA @ np.linalg.inv(SuDiag**(1/2))
        if matrix == 'vad':
            R = SuPCA
        
        if C == -np.inf:
            C = POETCmin(Y,K,thres,matrix)+0.1
            
        uu = np.zeros([p,p,n])
        roottheta = np.zeros([p,p])
        lbd = np.zeros([p,p])
        
        for i in range(p):
            for j in range(i): # symmetric matrix
                uu[i,j,] = uhat[i,] * uhat[j,]
                roottheta[i,j] = np.std(uu[i,j,])
                lbd[i,j] = roottheta[i,j]*rate*C
                lbd[j,i] = lbd[i,j]
                
        Rthresh = np.zeros(0,p,p)
        
        if thres == 'hard':
            for i in range(p):
                for j in range(i):
                    if np.abs(R[i,j]) < lbd[i,j] and j < i:
                        Rthresh[i,j] = 0
                    else:
                        Rthresh[i,j] = R[i,j]
                    Rthresh[j,i] = Rthresh[i,j]
        # TO-DO: soft, scad
        
        SigmaU = np.zeros(0,p,p)
        if matrix == 'cor':
            SigmaU = SuDiag^(1/2) @ Rthresh * SuDiag^(1/2)
        if matrix == 'vad':
            SigmaU = Rthresh
            
        SigmaY = SigmaU + Lowrank
        
        result = {'SigmaU':SigmaU,
                  'SigmaY':SigmaY,
                  'factors':F.t,
                  'loadings':LamPCA}
        return result
    
    
    