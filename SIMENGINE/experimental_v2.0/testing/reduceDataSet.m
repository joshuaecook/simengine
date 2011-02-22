function reduced = reduceDataSet(dataset)

num_points = 100;
fields = fieldnames(dataset);
reduced = struct();
for i=1:length(fields)
    data_mat = dataset.(fields{i});
    if size(data_mat,1) > num_points
        if size(data_mat,2) == 1
            reduced.(fields{i}) = interp1(1:size(data_mat,1),data_mat,1:num_points);
        else
            min_t = data_mat(1,1);
            max_t = data_mat(end,1);
            new_data_mat = zeros(num_points,size(data_mat,2));
            new_data_mat(:,1) = linspace(min_t,max_t,num_points);
            for j=2:(size(data_mat,2))
                new_data_mat(:,j) = interp1(data_mat(:,1),data_mat(:,j),new_data_mat(:,1));
            end
            reduced.(fields{i}) = new_data_mat;
        end
    else
        reduced.(fields{i}) = dataset.(fields{i});
    end
end

end