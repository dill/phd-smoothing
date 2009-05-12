% matlab code to load and map the samples 

% this is designed to be executed in the dir with the sc toolbox in it

% add in the right directories
path(path,'sc')
path(path,'poly_stuff')


nsamples=1000


for i=1:nsamples
   % load each sample in order
   thisdata=csvread(strcat('../ramseysim/ramsey-',num2str(i),'.csv'),1,0)

   % make the data complex
   thisdata_complex=complex(thisdata(:,2),thisdata(:,3))

   % map the sample
   remappeddata=evalinv(f,thisdata_complex)


   % write back to a csv file
   csvwrite(strcat('../ramseysim/ramsey-mapped-',num2str(i),'.csv'),[real(remappeddata),imag(remappeddata)])


end


