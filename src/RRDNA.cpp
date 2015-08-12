#include <Rcpp.h>
#include <stdlib.h>
using namespace Rcpp;
using namespace std;

inline int rr(const char c) {
    switch(c) {
    case 'A': case 'a': return 1;
    case 'T': case 't': return 2;
    case 'C': case 'c': return 3;
    case 'G': case 'g': return 4;
    default: return 0;
    }
}

//' Title: Convert DNA sequences to Reduced Representation format
//' 
//' @description This function receives a vector of strings (character vector) 
//' containing DNA sequences and returns a vector of strings containing 
//' codified DNA.
//' @param dnas Vector of DNA sequences (character vector).
//' @return Vector of DNA converted to reduced representation format 
//' (character vector).
//' @note This function is used internally to compute top over-represented 
//' reads and to store in RqcResultSet objects (per file top reads).
//' @seealso \code{\link{perFileTopReads}}
//' @author Welliton Souza
//' @examples
//' dna <- "ATCGNATCGTA"
//' dna.converted <- toRRDNA(dna)
//' nchar(dna)
//' nchar(dna.converted)
//' @export
// [[Rcpp::export]]
StringVector toRRDNA(StringVector dnas) {
    StringVector rrdnas(dnas.size());
    vector<char>::iterator irrdna;
    vector<char> rrdna;
    R_len_t i, j;
    string dna;
    int mod;
    
    for(i = 0; i < dnas.size(); ++i) {
        dna = dnas[i];
        rrdna = vector<char>(dna.size() / 3 + 2);
        irrdna = rrdna.begin();
        mod = dna.size() % 3;
        *irrdna++ = mod + 1;
        
        if (dna.size() > 2) {
            for (j = 0; j < dna.size(); j+=3)
                *irrdna++ = 1 + rr(dna[j]) + 5 * rr(dna[j+1]) + 25 * rr(dna[j+2]);
        
            if (mod == 1)
                *irrdna = 1 + rr(dna[j-2]);
            else if (mod == 2)
                *irrdna = 1 + rr(dna[j-2]) + 5 * rr(dna[j-1]);
        } else {
            if (mod == 1)
                *irrdna = 1 + rr(dna[0]);
            else if (mod == 2)
                *irrdna = 1 + rr(dna[0]) + 5 * rr(dna[1]);
        }
        rrdnas[i] = string(rrdna.begin(), rrdna.end());
    }
    
    return rrdnas;
}

const char DNA[] = {'N', 'A', 'T', 'C', 'G'};

//' Revert codified DNA sequences to original DNA sequences.
//' 
//' @description This function receives a vector of strings containing codified DNA and 
//' returns a vector of string containing original DNA sequences.
//' @note This function is used internally to restore original DNA sequences 
//' stored in RqcResultSet objects (per file top reads).
//' @param rrdnas Vector of codified DNA (character vector).
//' @return Vector of original DNA sequences (character vector).
//' @seealso \code{\link{perFileTopReads}}
//' @author Welliton Souza
//' @examples
//' dna <- "ATCG"
//' dna.converted <- toRRDNA(dna)
//' dna.reverted <- fromRRDNA(dna.converted)
//' all.equal(dna, dna.reverted)
//' @export
// [[Rcpp::export]]
StringVector fromRRDNA(StringVector rrdnas) {
    StringVector dnas(rrdnas.size());
    vector<char> dna;
    vector<char>::iterator idna;
    string rrdna;
    R_len_t i, j;
    int mod, a, r1, r2, r3;
    
    for(i = 0; i < rrdnas.size(); ++i) {
        rrdna = rrdnas[i];
        dna = vector<char>((rrdna.size() - 1) * 3);
        idna = dna.begin();
        
        for(j = 1; j < rrdna.size(); ++j) {
            a = rrdna[j] - 1;
            
            r3 = a / 25;
            a -= r3 * 25;
            r2 = a / 5;
            r1 = a - r2 * 5;
            
            *idna++ = DNA[r1];
            *idna++ = DNA[r2];
            *idna++ = DNA[r3];
        }
        
        mod = rrdna[0] - 1;
        if (mod != 0) mod = 3 - mod;
        
        dnas[i] = string(dna.begin(), dna.end() - mod);
    }
    return dnas;
}

//' Distance matrix of the similarity between the DNA sequences.
//' 
//' @description This function receives a vector of strings representing codified DNA 
//' sequences and returns a integer matrix representing the similarities 
//' between all sequences from input vectors.
//' @note This function is used internally to compute data for rqcFileHeatmap 
//' function.
//' @param rrdnas Vector of codified DNA sequences (character vector).
//' @return Matrix \eqn{n x n}, where \eqn{n} is the length of the largest 
//' original DNA sequence. 
//' @seealso \code{\link{rqcFileHeatmap}}
//' @author Welliton Souza
//' @examples
//' dna1 <- toRRDNA("atcgn")
//' dna2 <- toRRDNA("atcga")
//' matdist(c(dna1, dna2))
//' @export
// [[Rcpp::export]]
IntegerMatrix matdist(StringVector rrdnas) {
    IntegerMatrix ress(rrdnas.size(), rrdnas.size());
    string rrdna1, rrdna2;
    string::const_iterator c1, c2;
    int r, r1, r2, r3, s, m, d;
    R_len_t i, j;
    
    for(i = 0; i < rrdnas.size() - 1; ++i) {
        for (j = i; j < rrdnas.size(); ++j) {
            rrdna1 = rrdnas[i];
            rrdna2 = rrdnas[j];
            c1 = rrdna1.begin();
            c2 = rrdna2.begin();
            
            d = rrdna1.size() - rrdna2.size();
            if (d == 0) {
                s = 0; 
            } else if (d > 0) {
                m = *c1 - 1;
                if (m != 0) m = 3 - m;
                s = abs(d) * 3 - m;
            } else {
                m = *c2 - 1;
                if (m != 0) m = 3 - m;
                s = abs(d) * 3 - m;
            }
            
            c1++;
            c2++;
            while(c1 != rrdna1.end() && c2 != rrdna2.end()) {
                r = abs(*c1++ - *c2++);
                
                if (r != 0) {
                    r3 = r / 25;
                    r -= r3 * 25;
                    r2 = r / 5;
                    r1 = r - r2 * 5;
                    
                    if(r3!=0) s++;
                    if(r2!=0) s++;
                    if(r1!=0) s++;
                }
            }
            ress(i, j) = ress(j, i) = s;
        }
    }
    return ress;
}
