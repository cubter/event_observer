// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <unordered_map>
#include <vector>
#include <algorithm>
#include <omp.h>

using namespace Rcpp;

struct row_weight
{
    int32_t r;     // row
    double  w;     // weight

    row_weight() :
        r(0), w(0.1)
    { };
    row_weight(int row, double weight) :
        r(row), w(weight)
    { };
};

using rw_umap = std::unordered_map<String, std::vector<row_weight>>;

// I assign the weight to each row so that every observation on a certain date 
// will have a slightly bigger y-coordinate than the previous one. This helps draw
// the timeline.
// [[Rcpp::export]]
List assign_weights(const CharacterVector &dates, const IntegerVector &row_nums)
{
    rw_umap row_weights;
    
    IntegerVector::const_iterator rows_it     = row_nums.begin();
    CharacterVector::const_iterator dates_it  = dates.begin();
    
    std::vector<int> rows;
    std::vector<double> weights;
    std::vector<std::string> dates_out;
    
    for ( ; dates_it != dates.end(); dates_it++, rows_it++)
    {
        auto rw_it = row_weights.find(*dates_it);
        if (rw_it != row_weights.end())
        {
            rw_it->second.emplace_back(*rows_it, rw_it->second.back().w + 0.1);
            continue;
        }

        row_weights.try_emplace(*dates_it, std::vector<row_weight>{{*rows_it, 0.1}});
    }

    // Populating the output vectors to return a list.
    #pragma omp parallel for
    for (auto &rw : row_weights)
    {
        for (const auto i : rw.second)
        {
            rows.emplace_back(i.r);
            weights.emplace_back(i.w);
        }
        
        rw.second.clear();
    }
    
    return List::create(
        _["rows"] = rows,
        _["weights"] = weights);
}
