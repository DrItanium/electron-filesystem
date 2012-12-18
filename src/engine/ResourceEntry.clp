(defclass engine::ResourceEntry 
 "A class that is used in external files to define resources used by the
 engine"
 (is-a DataResource)
 (role concrete)
 (slot resource-type (type SYMBOL)))
 
