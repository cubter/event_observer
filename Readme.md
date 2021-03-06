## App

Available [here](http://194.87.186.139:3838/sample-apps/eventObserver/).

The app consists of three main parts:
1. preprocessor files (`preprocessor.R` & `preprocessor.sh`), which split the original file into smaller ones (more in "Thoughts on performance optimisation" section below),
2. redis uploader (`redis_uploader.R`) and 
3. the app itself: `server` & `UI` files & Shiny modules. 

`redis_uploader.R` constructs the names' indexes & uploads the data to the Redis. By constructing indexes I mean associating the events' coordinates, dates & times with the corresp. names and uploads the data to Redis.  

**Preprocessor scripts completely flush the existing DB** so that the data are uploaded every time from scratch. This takes a while , and it's a trade-off between update & app performances. For more, please see my last section, "P.S. On DB updates".

**Shiny app** itself is separated into modules. Those are responsible for rendering the timeline, map & statistics correspondingly. By rendering I mean dynamic rendering as well: *each time the user moves to another region on the map, the timeline gets redrawn to include only the points within the map bounds*. That said, the map's points are static, while the timeline's ones are not. 
I have spent much time trying to debug the problem with passing the map bounds out of the module to "normal" Shiny. 

The only essential non-modularised part remained is the *one processing the data from DB*. I experienced issues when trying to modularise it, so I had to refuse from this idea. Moreover, this part has no UI, and shiny modules, as far as I know, should always have the UI & server counterparts. 

For the plots' visualisation I used `plotly`.

As for the UI part, in some places I used `shinyWidgets` & `bslib`. If the app loading performance is not acceptable, I could drop them. 

Also, if the user has selected species with more than 30 000 events, a notification is shown, saying that the rendering may take time.

Lastly, perhaps I should've followed the best practice & moved regular R functions to separate files, but I was just lazy to do it. :)

### What I haven't achieved
- Tests. Yes, I know how important they are, however I didn't have enough time to write them. For an app to be used in prod., I'd def. use them. 

- Proper exception handling: I should have used `try`/`tryCatch` statements extensively. But once again: it's about time. 

- You may notice that the map's points' rendering tends to be slow once the number of events is equal to tens of thousands. Hence, I'm showing both a progress bar & a notification, so that the user doesn't think, the app has suddenly broken. Besides, two solutions for this I've been able to found: 
	1. As a workaround, I've enabled clustering of nearby points by default. It seems, it makes the rendering faster.
	2. Second, one could use `mapdeck` instead of leaflet. Mapdeck relies on extensive usage of `webGL`, which makes rendering significantly faster, but the drawback is that it uses Mapbox & requires its API token. Because I don't know, whether a proprietary solution is appropriate for this app, I had to refuse from this approach.  

## Data
I store all the data in a `Redis` instance. The motivation for this was that Redis is fast, and the data, on the other side, are not large.  

The structure is as follows:
- `list`s \<scientific name: coordinates\> (coord. are pasted together as a string)
- `list`s \<scientific name: dateTimes\> (dates & time are pasted together as a string)
- `hash`es \<vernacular name: scientific name\>
- `sorted set`s \<year: number of observations\>

Redis lists are linked lists, so inserting a new element takes O(1) time. This is a desired DS for storing coordinates, dates & times in the sense of insertion optimisation, but this also makes retrieving specific values impossible. 

As one may see, if the user selects a vernacular name, we firstly have to extract the corresp. scientific name from the hash. Only then can we obtain the events' data. There's an obvious drawback of this approach: I've to address the DB twice, which essentially means 2x RTTs and 2x read operations. On the flip side is an optimised storage, which was my primary motivation. 

#### Why Redis, why not SQL or read directly from file?
Well, it would be inefficient to read even a 2 GB file every time the app is loaded. Constructing indexes, even considering that environments are faster than named lists, does take a lot of time, and without them the search would be rel. inefficient. Of course, I could store the constructed indexes on disk, but that'd still be slower. 

I could basically use any RDBMS, but I didn't see the need to. Redis is relatively fast, the data required by the app are not huge, and the business requirements, IMO, fit well into usage of a key-value store. But it's also a matter of choice :).
The only drawback is that I can not pass the filtering of dates to Redis, and I've got to do it in the code, having prior transformed extracted date strings to `double`s. But again, it's performance vs storage efficiency. 

## Extra assignments

### Beautiful UI skill
Although I didn't handle CSS directly except in rare cases, I tried to make the app look better than by default. Little visualisation improvements mostly concern the plots, but I also used `shinyWidgets` & `bslib`. Bslib makes an app load a bit slower, hence if the speed is not acceptable, it can be switched off.

### Thoughts on performance optimisation
I tried to optimise the performance everywhere I could, and used the whole dataset instead of the Poland-only one. Still, it does take time to point hundreds of thousands of events on the map & timeline, which means it's a rendering issue. I believe, there's not much that can be done about it, except for using other libraries like `mapdeck`.

Besides, I shrinked the initial file, having only the required columns left. This helped reduce the size by as much as 18 Gb. As a result, I obtained a rel. not large data set, which fitted well into a Redis database, assuming the server has at least 2 GB RAM. Had I had to deal with the whole initial file, I'd have gone a more typical SQL way & used Redis for caching the most requested species.

I've written `C++` code to assign weights to timeline's points, so that they've different y-coordinates. Although R's envrironments are fast, STL's `unordered_map`s are faster, and this helped improve the performance for species with numerous events. 
> Despite this, sourcing a C++ file does take time initially, and it seems it's a bottleneck right 
> now during the app's launch.

As regards maps, because there're species with myriads of events, I render them with `webGL`. The drawback is that old browsers do not support it. An optimal solution here would be detecting, if the browser supports WebGL and using it then, reverting to the old CPU rendering otherwise, but I'm not aware, that it can be implemented other than writing JS code.

As regards plots, I use `plotly`'s `canvas` option to improve rendering. Still, hundreds of thousands of events render rel. slow.

Lastly, I use Redis pipelining features which are supported by `Redux` (R library for Redis). This helps significantly improve the uploading performance. Other option would be Redis scripting, but considering that I typically have no more than 5-10k unique scientific names per file, I decided to follow the former way.  
 
Besides, because Redis de-facto stores every value as a string, when extracting dates, I've firstly to convert them to `double`s to enable filtering. `Lubridate` here has helped vastly improve the performance (0.9s vs 7s for 500K date strings) as compared to the built-in `as.Date()`. 

Caching. I've added memoisation of the results of the most time-intesive functions' using `memoise`. Caching is implemented on the appliance level, which means it's shared across the users' sessions. Besides, I use disk cache, i.e. caching also persists across different processes. 
It'd be more optimal to log selection of species to understand what species are requested most (my biological assumption is that such distribution will be close to normal), but that'd be too complicated, as `bindCache` now only supports LRU & FIFO types. 

However, I assume that some code blocks still remain unoptimised. For example, I could try to parallelise the processing of `occurrence` files. But that'd require rewriting the existing loops to remove data dependencies.  

### Infrastructure 
I've deployed the app on my own VPS (2 CPUs, 2 GB RAM) using `Shiny server`. I preferred Shiny server mostly because it allows a faster deployment. Though the Shinyproxy's approach seems more modern as it relies on containers.

Please, note that the instances of Shiny server & Redis out there have default configurations. Had I dealt with a production-ready app, I'd have definitely customised their security settings beyond simple firewalling. I'd have also probably separated the app's server from the DB's one. 

### Javascript
Not used, though, I'm sure, working with `WebSockets` would constitute the app's performance improvement.

## Last: features I'd love to add, had I more time
- *Selection option for locations*: user could choose a location from a human-readable list (e.g. Germany, Poland, UK, Europe, World)
	- difficulty for me: easy;
- *Coloring countries*: countries would be colored acc. to the number of events: the more events, the more intensive the color is;
	- difficulty for me: easy;
- *Dark theme switcher*
	- difficulty for me: rel. easy, but requires dark version of the map;
- *Flashing the clicked map's point on the timeline* 
	- difficulty for me: not sure but it seems there's a built-in workaround for this;
- Supplying *more information about the events*, like links to multimedia files, links to occurrence IDs etc.
	- difficulty for me: easy;
- Option to *select timeline's units* (year, month, decade)
	- difficulty for me: easy.
	
## P.S. On DB updates

Despite that R's evironments are the fastest implementation of hash tables available in R, the avg. speed of processing a .csv file with 1500000 lines is several minutes, making up between an hour and two for the whole data set. This is a barely acceptable speed, provided the data are updated frequently; that's why my next step here would also be switching to `Rcpp` & using STL's unordered maps.

I could also send perhaps 10x more commands to Redis without prior construction of the indexes at all, based on the assumption that Redis will handle hash insertions quicker. But that'd require significantly more requests to the DB.
